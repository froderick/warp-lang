(ns jib.compile
  "For a start, just see what clojure code that spits out basic assembly for a
   main method looks like."
  (:require [clojure.spec.alpha :as s]
            [clj-antlr.core :as antlr]
            [clojure.pprint :refer [pprint]]
            [clojure.walk :as walk]
            [clojure.zip :as zip]
            [clojure.test :as test]
            [jib.il :as il]
            [jib.rt :as rt]
            [jib.analyze :as an]))

(defn fail [& msgs]
  (throw (Exception. (apply str "compilation failed: " msgs))))


;; compiler behavior ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ast-seq
  "Useful for filtering the analyzer's resulting ast which contains both lists and maps."
  [exprs]
  (tree-seq #(or (seq? %)
                 (map? %))
            #(if (seq? %)
               (identity %)
               (interleave (keys %) (vals %)))
            exprs))

(defn calc-stack-space
  "Computes required stack size based purely on the number of ints to be stored.
   Stack increments must be 16-byte aligned."
  [args]
  (let [num-args (count args)
        pointers-allocated (if (odd? num-args) (inc num-args) num-args)]
    (* pointers-allocated rt/pointer-length)))

(defn calc-stack-addrs
  "For a given set of expressions, computes stack addresses for their results
   relative to the stack pointer."
  [exprs]
  (->> exprs
       (map-indexed
        (fn [idx expr]
          [expr (il/reg-addr :rsp (* idx rt/pointer-length))]))
       (into {})))

(defn with-stack-space
  "Sets up and tears down 16-byte aligned stack space. Useful for when you have
   n things you know you need to evaluate and push to the stack in sequence. Calls
   `f` in between, with a map of thing->stack-address."
  [exprs f]
  (let [plus-stack-size (il/literal (calc-stack-space exprs))
        expr-addrs (calc-stack-addrs exprs)]
    [; make room for args on stack plus any 16-byte alignment padding
     (when-not (empty? exprs)
       (il/subq plus-stack-size (il/reg-addr :rsp)))

     ; call nested behavior with allocated space
     (f expr-addrs)

     ; evict scratch stack space
     (when-not (empty? exprs)
       (il/addq plus-stack-size (il/reg-addr :rsp)))]))

;; compiling constants

(defn constant? [f]
  (= (:form f) :constant))

(defn collect-constants [exprs]
  (->> exprs
       ast-seq
       (filter constant?)))

(defn raw-constant-id
  [constant-id]
  (symbol (str constant-id "_raw")))

(defn compile-constants-data
  "Creates the data section that contains the raw constant values."
  [constants]
  [(il/line-comment "decl raw constant data")
   (il/section
    :text
    (for [{:keys [type value constant-id]} constants]
      [(il/label (raw-constant-id constant-id))
       (case type
         :number  (il/quad value)
         :nil     (il/quad 0)
         :boolean (il/quad (if (true? value) 1 0))
         :string  (il/cstring-data value))]))
   (il/line-comment "decl initialized constant pointers")
   (il/section
    :rwdata
    (for [{:keys [type value constant-id]} constants]
      [(il/label constant-id) (il/quad 0)]))])

(defn compile-constants
  "Uses the raw constant data section values to implement wrapped runtime values."
  [constants]
  [(il/line-comment "allocate constants")
   (for [{:keys [type value constant-id]} constants]
     (let [raw-addr (il/label-addr (raw-constant-id constant-id) :rip)
           addr (il/label-addr constant-id :rip)]
       [(case type
          :number (rt/alloc-int-addr raw-addr)
          :nil (rt/alloc-nil)
          :boolean (rt/alloc-bool-addr raw-addr)
          :string (rt/alloc-str-addr raw-addr))
        (il/movq (il/reg-addr :rax) addr)]))])

;; compiling lookup table for functions used as fn-callables

(defn fn-callable? [f]
  (= (:form f) :fn-callable))

(defn collect-fn-callables
  [exprs]
  (->> exprs
       ast-seq
       (filter fn-callable?)
       (into []))) ; identified by index

(defn fn-callables-name-to-index
  "Used when the compiler is trying to map names to indexes"
  [fn-callables]
  (->> fn-callables
       (map-indexed (fn [idx fn-callable]
                      [(:name fn-callable) idx]))
       (into {})))

(defn format-alts
  [^Character c]
  (case (str c)
    "+" "plus"
    "/" "div"
    "=" "equals"
    "!" "bang"
    "%" "percent"
    "*" "star"
    "^" "carrot"
    ">" "gt"
    "<" "lt"
    "?" "question"
    "&" "and"
    "-" "dash"
    (str c)))

(defn format-func-symbol
  "Escapes func name as a valid assembly symbol."
  [^String n]
  (str (->> (seq n)
            (map format-alts)
            (apply str))))


(def fn-callable-lookup-table-pointer :fn_callable_lookup_table_pointer)

(defn compile-fn-callable-lookup-table
  "Creates the data section that contains the raw constant values."
  [fn-callables]
  (when-not (empty? fn-callables)
    [(il/line-comment "decl fn-callable lookup table data")
     (il/section
      :text
      [(il/label fn-callable-lookup-table-pointer)
       (il/quad-array (map-indexed (fn [idx fn-callable]
                                     (format-func-symbol (:name fn-callable)))
                                   fn-callables))])]))

(declare compile-expr)

(defn compile-fn-callable
  [{:keys [fn-callables-name-to-index] :as ctx} {:keys [name] :as fn-callable}]
  (let [table-idx (get fn-callables-name-to-index name)]
    [(il/leaq (il/label-addr fn-callable-lookup-table-pointer :rip) (il/reg-addr :rax))
     (when-not (zero? table-idx)
       (il/addq (il/literal table-idx) (il/reg-addr :rax)))
     (rt/alloc-fn (il/reg-addr :rax))]))

;; compiling function definitions

(defn binding? [f]
  (boolean (#{:let :loop} (:form f))))

(defn collect-func-args
  "Creates a mapping of arg id to the place in memory it will be located."
  [{:keys [args]}]
  (->> args
       reverse
       (map-indexed (fn [idx arg]
                      (let [return-pointer-length 8
                            args-length (* (- (count args) idx) 8)
                            arg-offset (+ args-length return-pointer-length)]
                        [(:id arg) (il/reg-addr :rbp arg-offset)])))
       (into {})))

(defn collect-func-binding-ids
  "Returns a list of ids for bindings discovered in a particular expression tree."
  [exprs]
  (->> exprs
       ast-seq
       (filter binding?)
       (map :bindings)
       flatten
       (map :id)))

(defn collect-func-locals
  "Creates a mapping of local id to the place in memory it will be located."
  [exprs]
  (let [first-local-offset (- 0 rt/pointer-length)] ; skip past %rbp
    (->> exprs
         collect-func-binding-ids
         (map-indexed (fn [idx id]
                        (let [local-offset (- first-local-offset (* idx rt/pointer-length))]
                          [id (il/reg-addr :rbp local-offset)])))
         (into {}))))

(defn compile-func
  [{:keys [lookup] :as ctx} {:keys [name args exprs] :as func}]

  (let [locals-lookup-table (collect-func-locals exprs)]

    (il/section :text [; function entry point
                        (il/label (format-func-symbol name))

                        ; prologue
                        (il/pushq (il/reg-addr :rbp))
                        (il/movq (il/reg-addr :rsp) (il/reg-addr :rbp))

                        ; make room for locals, align stack to 16 bytes
                        (let [local-stack-size (calc-stack-space locals-lookup-table)]
                          (il/subq (il/literal local-stack-size) (il/reg-addr :rsp)))

                        (il/line-comment "function body")
                        (if (empty? exprs)
                          (il/movq (il/literal 0) (il/reg-addr :%rax))
                          (let [args-lookup-table (collect-func-args func)
                                lookup (merge args-lookup-table locals-lookup-table)
                                ctx (assoc ctx :lookup lookup)]
                            (map #(compile-expr ctx %) exprs)))

                        ; epilogue
                        (il/leave)
                        (il/ret)])))

(defn compile-if
  [ctx {:keys [test if-branch else-branch]}]
  (let [else-label (il/label (gensym "Lelse_branch"))
        done-label (il/label (gensym "Ldone_branch"))]
    [(il/line-comment "iftest")
     (compile-expr ctx test)
     (rt/truthy (il/reg-addr :rax))
     (il/cmpq (il/literal 0) (il/reg-addr :rax))
     (il/je else-label)
     (il/line-comment "ifbranch")
     (compile-expr ctx if-branch)
     (il/jmp done-label)
     (il/line-comment "elsebranch")
     else-label
     (if else-branch
       (compile-expr ctx else-branch)
       (rt/alloc-nil))
     done-label]))

(defn num-reduce
  "(f state-addr n-addr) -> returns reduce instructions"
  [ctx exprs f]
  (cond

    (empty? exprs)
    (rt/alloc-int 0)

    (= (count exprs) 1)
    (compile-expr ctx (first exprs))

    :else
    (with-stack-space
      exprs
      (fn [args-addrs]
        [; eval args, unwrap int pointers, move into stack
         (for [expr exprs]
           [(compile-expr ctx expr)
            (rt/val-int (il/reg-addr :rax))
            (il/movq (il/reg-addr :rax) (get args-addrs expr))])

         ; move first arg into rax
         (il/movq (get args-addrs (first exprs)) (il/reg-addr :rax))

         ; reduce the rest into rax via rdi
         (for [expr (rest exprs)]
           (let [state-addr (il/reg-addr :rax)
                 n-addr (il/reg-addr :rdi)]
             [(il/movq (get args-addrs expr) n-addr)
              (f state-addr n-addr)]))

         ; allocate a new int to return
         (rt/alloc-int-addr (il/reg-addr :rax))]))))

(defn builtin-plus
  [ctx exprs]
  (num-reduce ctx exprs (fn [state-addr n-addr]
                             (il/addq n-addr state-addr))))

(defn builtin-minus
  [ctx exprs]
  (num-reduce ctx exprs (fn [state-addr n-addr]
                             (il/subq n-addr state-addr))))

(defn builtin-multiply
  [ctx exprs]
  (num-reduce ctx exprs (fn [state-addr n-addr]
                             (il/imulq n-addr state-addr))))

(defn builtin-divide
  [ctx exprs]
  (num-reduce ctx exprs (fn [state-addr n-addr]
                             [; low bits already set by num-reduce in %rax
                              (il/cdq) ; sign-extend %rax into %rdx
                              (il/idivq n-addr)])))

(defn builtin-modulus
  [ctx exprs]

  (when-not (= (count exprs) 2)
    (fail "mod requires exactly 2 arguments"))

  (num-reduce ctx exprs (fn [state-addr n-addr]
                             [; low bits already set by num-reduce in %rax
                              (il/cdq) ; sign-extend %rax into %rdx
                              (il/idivq n-addr)
                              (il/movq (il/reg-addr :rdx) state-addr)])))

(defn num-compare
  [ctx exprs il-jump]

  (cond

    (empty? exprs)
    (fail "num-compare requires at least 1 argument")

    (= (count exprs) 1)
    (rt/alloc-bool true)

    :else
    (with-stack-space
      exprs
      (fn [args-addrs]

        (let [criteria-not-met (il/label)
              done (il/label)]

        [; eval args, unwrap int pointers, move into stack
         (for [expr exprs]
           [(compile-expr ctx expr)
            (rt/val-int (il/reg-addr :rax))
            (il/movq (il/reg-addr :rax) (get args-addrs expr))])

         ; move first arg into rax
         (il/movq (get args-addrs (first exprs)) (il/reg-addr :rax))

         ; emit a sequence of comparisions over the rest of the args
         (for [expr (rest exprs)]
           (let [state-addr (il/reg-addr :rax)
                 n-addr (il/reg-addr :rdi)]
             [(il/movq (get args-addrs expr) n-addr)
              (il/cmpq n-addr state-addr)
              (il-jump criteria-not-met)
              (il/movq n-addr state-addr)]))
         (rt/alloc-bool true) ; land here if condition is met
         (il/jmp done)

         criteria-not-met ; jump here if condition is not met
         (rt/alloc-bool false)
         done])))))

(defn builtin-lt
  [ctx exprs]
  (num-compare ctx exprs il/jge))

(defn builtin-gt
  [ctx exprs]
  (num-compare ctx exprs il/jle))

(defn builtin-lte
  [ctx exprs]
  (num-compare ctx exprs il/jg))

(defn builtin-gte
  [ctx exprs]
  (num-compare ctx exprs il/jl))

(defn builtin-eq
  [ctx [a b :as exprs]]

  (when-not (= (count exprs) 2)
    (fail "eq requires exactly 2 arguments"))

  (with-stack-space
    exprs
    (fn [args-addrs]

      [; eval args, move into stack
       (for [expr exprs]
         [(compile-expr ctx expr)
          (il/movq (il/reg-addr :rax) (get args-addrs expr))])

       ; determine equality
       (rt/val-equals-addr
        (get args-addrs a)
        (get args-addrs b))

       ; make a bool value from it
       (rt/alloc-bool-addr (il/reg-addr :rax))
       ])))

(defn builtin-cons
  [ctx [x xs :as exprs]]

  (with-stack-space
    exprs
    (fn [args-addrs]

      [; eval args, move into stack

       (for [expr exprs]
         [(compile-expr ctx expr)
          (il/movq (il/reg-addr :rax) (get args-addrs expr))])

       (let [x-addr (get args-addrs x)
             xs-addr (get args-addrs xs)]
         (rt/val-cons x-addr xs-addr))])))

(defn builtin-count
  [ctx [expr :as exprs]]

  (when-not (= (count exprs) 1)
    (fail "count requires exactly 1 arguments"))

  [(compile-expr ctx expr)
   (rt/val-count (il/reg-addr :rax))])

(defn builtin-first
  [ctx [expr :as exprs]]

  (when-not (= (count exprs) 1)
    (fail "first requires exactly 1 arguments"))

  [(compile-expr ctx expr)
   (rt/val-first (il/reg-addr :rax))])

(defn builtin-rest
  [ctx [expr :as exprs]]

  (when-not (= (count exprs) 1)
    (fail "rest requires exactly 1 arguments"))

  [(compile-expr ctx expr)
   (rt/val-rest (il/reg-addr :rax))])

(defn builtin-println
  [ctx exprs]
  [(compile-expr ctx (first exprs))
   (rt/println-addr (il/reg-addr :rax))])

(defn func? [f]
  (= (:form f) :func))

(defn collect-funcs [exprs]
  (->> exprs
       ast-seq
       (filter func?)
       (map #(select-keys % [:name :args :variadic?]))))

(defn get-func-info
  [{:keys [funcs] :as ctx} func-name]
  (->> funcs
       (filter #(= (:name %) func-name))
       first))

(defn require-func-info [ctx func-name]
  (let [info (get-func-info ctx func-name)]
    (when (nil? info)
      (fail "cannot find function in ctx: " func-name ctx))
    info))

(defn compile-fn-call-args-non-variadic
  [ctx func-name args call-instructions]
  (with-stack-space
    args
    (fn [args-addrs]
      [(for [arg args]
        [(compile-expr ctx arg)
         (il/movq (il/reg-addr :rax) (get args-addrs arg))])
       call-instructions])))

(defn build-variadic-args-list
  [ctx exprs list-addr]

  [(rt/alloc-list)
   (il/movq (il/reg-addr :rax) list-addr)

   (for [expr exprs]
     [(compile-expr ctx expr)
      (rt/val-cons (il/reg-addr :rax) list-addr)
      (il/movq (il/reg-addr :rax) list-addr)])])

(defn compile-fn-call-args-variadic
  [ctx func-name args call-instructions]
  (let [num-defined-args (-> (get-func-info ctx func-name) :args count)
        num-normal-args (dec num-defined-args)
        normal-args   (take num-normal-args args)
        variable-args (drop num-normal-args args)]

    (with-stack-space
      (->> normal-args (cons :varargs) reverse)
      (fn [args-addrs]
        [(for [arg normal-args]
           [(compile-expr ctx arg)
            (il/movq (il/reg-addr :rax) (get args-addrs arg))])
         (build-variadic-args-list ctx (reverse variable-args) (:varargs args-addrs))
         call-instructions]))))

(defn compile-fn-call
  "Evaluates each argument, pushes it on the stack in reverse order.
   Calls the function whose results go into %rax, and then pops the
   arguments off the stack."
  [{:keys [lookup] :as ctx} {:keys [name args dynamic? binding]}]
  (let [{:keys [variadic?]} (get-func-info ctx name)
        call-instructions (if-not dynamic?
                            (il/call (format-func-symbol name))
                            [(rt/val-fn-ptr (get lookup (:id binding)))
                             (il/call-addr "*(%rax)")])]
    [(if-not variadic?
       (compile-fn-call-args-non-variadic ctx name args call-instructions)
       (compile-fn-call-args-variadic ctx name args call-instructions))
     ]))

(defn make-bindings
  [{:keys [lookup] :as ctx} bindings]
  (for [{:keys [name id expr]} bindings]
    [(il/line-comment (str "let " name))
     (compile-expr ctx expr)
     (il/movq (il/reg-addr :rax) (get lookup id))]))

(defn clear-bindings
  [{:keys [lookup] :as ctx} bindings]
  (for [{:keys [name id expr]} bindings]
    (il/movq (il/literal 0) (get lookup id))))

(defn compile-let
  [ctx {:keys [bindings exprs]}]
  [(il/line-comment "let")
   (make-bindings ctx bindings)
   (map #(compile-expr ctx %) exprs)
   (clear-bindings ctx bindings)
   (when (zero? (count exprs))
     (rt/alloc-nil))])

(defn compile-loop
  [ctx {:keys [bindings exprs loop-start-label]}]

  [(make-bindings ctx bindings)
   (il/label loop-start-label)
   (map #(compile-expr ctx %) exprs)
   (clear-bindings ctx bindings)
   (when (zero? (count exprs))
     (rt/alloc-nil))])

(defn compile-recur
  [{:keys [lookup] :as ctx} {:keys [exprs loop-start-label loop-bindings]}]

  [(with-stack-space
     exprs
     (fn [args-addrs]

       [; eval args, move into stack
        (il/line-comment "calc new loop binding values")
        (for [expr exprs]
          [(compile-expr ctx expr)
           (il/movq (il/reg-addr :rax) (get args-addrs expr))])

        ; replace original loop bindings from temp stack space
        (il/line-comment "update loop bindings")
        (for [[id expr] (zipmap loop-bindings exprs)]
          [(il/movq (get args-addrs expr) (il/reg-addr :rax))
           (il/movq (il/reg-addr :rax) (get lookup id))])]))

   ; jump to start of loop
   (il/jmp (il/label loop-start-label))]) ;TODO: to avoid leaking stack, before we jump we should drop back to wherever
                                          ;      the stack was before we started looping

(defn compile-binding-ref
  [{:keys [lookup] :as ctx} {:keys [binding-id]}]
  (il/movq (get lookup binding-id) (il/reg-addr :rax)))

(defn compile-constant-ref
  [ctx {:keys [constant-id type value]}]
  (case type
    (il/movq (il/label-addr constant-id :rip) (il/reg-addr :rax))))

(defn compile-expr
  "General-purpose epxression evaluation function. Whatever this function
   generates should always put the resulting computed value in %ret."
  [ctx expr]

  ;; TODO: handle list of expressions here?

  (case (:form expr)
    :func (compile-func ctx expr) 
    :if (compile-if ctx expr)
    :let (compile-let ctx expr)
    :loop (compile-loop ctx expr)
    :recur (compile-recur ctx expr)
    :builtin (let [{:keys [name args]} expr]
               (case name
                 "builtin-plus"    (builtin-plus ctx args)
                 "builtin-minus"   (builtin-minus ctx args)
                 "builtin-mult"    (builtin-multiply ctx args)
                 "builtin-div"     (builtin-divide ctx args)
                 "builtin-mod"     (builtin-modulus ctx args)
                 "builtin-eq"      (builtin-eq ctx args)
                 "builtin-lt"      (builtin-lt ctx args)
                 "builtin-gt"      (builtin-gt ctx args)
                 "builtin-lte"     (builtin-lte ctx args)
                 "builtin-gte"     (builtin-gte ctx args)
                 "builtin-println" (builtin-println ctx args)
                 "builtin-cons"    (builtin-cons ctx args)
                 "builtin-count"   (builtin-count ctx args)
                 "builtin-first"   (builtin-first ctx args)
                 "builtin-rest"    (builtin-rest ctx args)
                 (fail "no builtin matches " name)))
    :fn-call (compile-fn-call ctx expr)
    :fn-callable (compile-fn-callable ctx expr)
    :binding-ref (compile-binding-ref ctx expr)
    :constant (compile-constant-ref ctx expr)
    (fail "not a recognized expr type: " expr)))

(defn unwrap-exit-code
  "Inspects the type of the value referenced in %rax. If it is an int,
   it unwraps the raw int value and stores it in %rax. If it is not an
   int, it sets %rax to 0."
  []
  (let [T_INT (il/literal 3)
        rax (il/reg-addr :rax)
        else-label (il/label)
        done-label (il/label)]

    [(il/line-comment "unrap main return value")

     (il/pushq rax) ; store rax

     (rt/val-type rax) ; test rax
     (il/cmpq T_INT rax)
     (il/jne else-label)

     (il/popq rax) ; if rax was int, unrwap
     (rt/val-int rax)
     (il/jmp done-label)

     else-label ; else replace with -1
     (il/popq rax)
     (il/movq (il/literal 0) rax)

     done-label]))

(defn collect-gen-funcs
  [exprs]
  (->> exprs
       ast-seq
       (map :gen-func)
       (filter identity))) 

(defn fn-callables-name-to-index
  "Used when the compiler is trying to map names to indexes"
  [fn-callables]
  (->> fn-callables
       (map-indexed (fn [idx fn-callable]
                      [(:name fn-callable) idx]))
       (into {})))

(defn compile-all
  [exprs]
  (let [constants (collect-constants exprs)
        funcs (collect-funcs exprs)
        fn-callables (collect-fn-callables exprs)
        gen-funcs (collect-gen-funcs exprs)
        lookup {}
        ctx {:lookup lookup
             :funcs funcs
             :fn-callables-name-to-index (fn-callables-name-to-index fn-callables)}
        entry-point-label-name "_main"]

    [; data sections
     (rt/entry-point-data)
     (compile-constants-data constants)
     (compile-fn-callable-lookup-table fn-callables)

     ; generated compiled code section (anonymous functions)
     (il/section :text (map #(compile-func ctx %) gen-funcs))

     ; compiled code section
     (il/section :text (map #(compile-expr ctx %) exprs))

     ; entry point section
     (il/section
      :text
      [; function entry point
       (il/label entry-point-label-name)

       ; prologue
       (il/pushq (il/reg-addr :rbp))
       (il/movq (il/reg-addr :rsp) (il/reg-addr :rbp))

       ; runtime init
       (rt/entry-point-begin entry-point-label-name)
       (compile-constants constants)

       (if (empty? exprs)

         ; exit value 0
         (il/movq (il/literal 0) (il/reg-addr :rax))

         [; call -main function
          (compile-fn-call ctx {:name "main" :args []}) ; TODO: pass cmdline args
          ; interpret the returned value as an exit value
          (unwrap-exit-code)])

       ; destroy runtime, passthrough %rax
       (rt/entry-point-end)

       ; epilogue
       (il/leave)
       (il/ret)])]))

(defn compile-ast
  "Take a parsed ast, emit the x86_64 assembly."
  [ast]
  (let [exprs (->> ast
                   an/mapify-ast
                   (map an/analyze-expr))]
    (compile-all exprs)))

(defn doeval

  ([text]
   (doeval text nil))

  ([text {:keys [ast? analyze? asm?] :or {ast? false analyze? false asm? false}}]

   (when ast?
     (pprint (->> text
                  (antlr/parse an/parser))))

   (when analyze?
     (pprint (->> text
                  (antlr/parse an/parser)
                  an/mapify-ast
                  (map an/analyze-expr))))

  (let [with-stdlib (str (slurp "std.jib") "\n\n" text)

        asm (->> with-stdlib
                 (antlr/parse an/parser)
                 compile-ast
                 il/emit)
        asm (str asm "\n")
        asm-file "main.s"
        executable "main"
        base-dir (System/getProperty "user.dir")]

    (when asm?
      (println asm))

    (spit (clojure.java.io/file asm-file) asm)

    (let [asm-results (clojure.java.shell/sh "cc" asm-file (str base-dir "/runtime/runtime.o")"-g" "-o" "main")]
      (when-not (zero? (:exit asm-results))
        (throw (ex-info (str "assembly failed:\n"
                             "\texit: " (:exit asm-results)
                             "\tout: " (:out asm-results)
                             "\terr: " (:err asm-results))
                        asm-results))))

    (clojure.java.shell/sh "./main"))))

(defn doeval-exit-code
  ([text]
   (-> (doeval text)
       :exit))
  ([text options]
   (-> (doeval text options)
       :exit)))

(comment 
  
  (doeval-exit-code "(func orig () 100)
                     (func main()
                       (let (doop orig)
                         (doop 1 2)))"
                    {:analyze? true :asm? true})

  (try 
    (->> "(func main()
            (let (doop joe
                  other-doop bob)
              (other-doop (doop 1 2))))"
         (antlr/parse an/parser)
         an/mapify-ast
         first
         an/analyze-expr
         collect-fn-callables
                                        ;fn-callables-name-to-index
         compile-fn-callable-lookup-table
         il/emit
         println)
    (catch clj_antlr.ParseError e (pprint @e))) 

)

(test/deftest test-compiler
  (test/is (= (doeval-exit-code
                "(func main ()
                   (if 1
                     10
                     20))")
              10)
           "if-branch")

  (test/is (= (doeval-exit-code
                "(func main ()
                   (if false
                     10
                     20))")
              20)
           "else-branch")

  (test/is (= (doeval-exit-code
               "(func hi (x y z)
                   (+ x y (+ z 200)))

                 (func main ()
                   (- (hi 1 2 3) 1))")
              205) 
           "fn-call")

  (test/is (= (doeval-exit-code
               "(func main ()
                   (let (a 123
                         b 100)
                     (let (c 1)
                       (+ 1 2)
                       (+ a b c))))")
              224) 
           "let")

  (test/is (= (doeval-exit-code
               "(func main () (+))")
              0) 
           "plus-empty")

  (test/is (= (doeval-exit-code
               "(func main () (+ 10))")
              10) 
           "plus-single")

  (test/is (= (doeval-exit-code
                "(func main () (+ 10 1 2 3))")
              16) 
           "plus")

  (test/is (= (doeval-exit-code
               "(func main () (- -10))")
              10) 
           "minus-single")

  (test/is (= (doeval-exit-code
               "(func main () (- 10 1 2 3))")
              4) 
           "minus")

  (test/is (= (doeval-exit-code
               "(func main () (*))")
              1) 
           "mult-empty")

  (test/is (= (doeval-exit-code
               "(func main () (* 10))")
              10) 
           "mult-single")

  (test/is (= (doeval-exit-code
               "(func main () (* 10 20))")
              200) 
           "mult")


  (test/is (= (doeval-exit-code
               "(func main () (/ 10))")
              10) 
           "div-single")

  (test/is (= (doeval-exit-code
               "(func main () (/ 100 20))")
              5) 
           "div")

  (test/is (= (doeval-exit-code
               "(func main () (if (= 11) 1 0))")
              1) 
           "eq-single")


  (test/is (= (doeval-exit-code
               "(func main () (if (= 11 11) 1 0))")
              1) 
           "eq")

  (test/is (= (doeval-exit-code
               "(func main () (if (= 10 11) 1 0))")
              0) 
           "eq-not")

  (test/is (= (doeval-exit-code
               "(func main () (if (> 11) 1 0))")
              1) 
           "gt-single")

  (test/is (= (doeval-exit-code
               "(func main () (if (> 12 11) 1 0))")
              1) 
           "gt")

  (test/is (= (doeval-exit-code
               "(func main () (if (> 11 11) 1 0))")
              0) 
           "gt-not")

  (test/is (= (doeval-exit-code
               "(func main () (if (< 11) 1 0))")
              1) 
           "lt-single")

  (test/is (= (doeval-exit-code
               "(func main () (if (< 10 11) 1 0))")
              1) 
           "gt-not")

  (test/is (= (doeval-exit-code
               "(func main () (if (< 12 11) 1 0))")
              0) 
           "lt-not")

  (test/is (= (doeval-exit-code
               "(func main () (if (>= 11) 1 0))")
              1) 
           "gte-single")

  (test/is (= (doeval-exit-code
               "(func main () (if (>= 11 11) 1 0))")
              1) 
           "gte-eq")

  (test/is (= (doeval-exit-code
               "(func main () (if (>= 12 11) 1 0))")
              1) 
           "gte")

  (test/is (= (doeval-exit-code
               "(func main () (if (>= 10 11) 1 0))")
              0) 
           "gte-not")

  (test/is (= (doeval-exit-code
               "(func main () (if (<= 11) 1 0))")
              1) 
           "lte-single")

  (test/is (= (doeval-exit-code
               "(func main () (if (<= 11 11) 1 0))")
              1) 
           "lte-eq")

  (test/is (= (doeval-exit-code
               "(func main () (if (<= 10 11) 1 0))")
              1) 
           "lte")

  (test/is (= (doeval-exit-code
               "(func main () (if (<= 12 11) 1 0))")
              0) 
           "lte-not")

  (test/is (= (doeval-exit-code
               "(func fuck (a b) a)
                (func main () (fuck 10 20))")
              10)
           "non-varidadic func first arg bound correctly")

  (test/is (= (doeval-exit-code
               "(func fuck (a b) b)
                (func main () (fuck 10 20))")
              20)
           "non-varidadic func second arg bound correctly")

  (test/is (= (doeval-exit-code
               "(func fuck (& args) (count args))
                (func main () (fuck 10 20))")
              2)
           "varidadic func no fixed args")

  (test/is (= (doeval-exit-code
               "(func fuck (a & args) a)
                (func main () (fuck 10 20))")
              10)
           "varidadic func first fixed arg")

  (test/is (= (doeval-exit-code
               "(func fuck (a & args) (count args))
                (func main () (fuck 10 20 30))")
              2)
           "varidadic func first fixed arg remaining variable args")

  (test/is (= (doeval-exit-code
               "(func main () (first (cons 10 20)))")
              10)
           "cons non-list first")

  (test/is (= (doeval-exit-code
               "(func main () (second (cons 10 20)))")
              20)
           "cons non-list second")

  (test/is (= (doeval-exit-code
               "(func main () (first (cons 50 (cons 10 ()))))")
              50)
           "cons list first")

  (test/is (= (doeval-exit-code
               "(func main () (second (cons 50 (cons 10 ()))))")
              10)
           "cons list second")

  (test/is (= (doeval-exit-code
               "(func main () (if (nil? (first ())) 1 0))")
              1)
           "first empty list")

  (test/is (= (doeval-exit-code
               "(func main () (if (nil? (second ())) 1 0))")
              1)
           "second empty list")

  (test/is (= (doeval-exit-code
               "(func main () (if false 1))")
              0)
           "if-else branch must allocate nil when not specified")

  (test/is (= (doeval-exit-code
               "(func main () (println \"one\" \"two\" \"three\"))")
              0)
           "println - should not explode")

  (test/is (= (doeval-exit-code
               "(func main () (if (not false) 20 40))")
              20)
           "not-false")

  (test/is (= (doeval-exit-code
               "(func main () (if (not true) 20 40))")
              40)
           "not-true")

  (test/is (= (doeval-exit-code
               "(func main () (if (not \"hi\") 20 40))")
              40)
           "not-string")

  (test/is (= (doeval-exit-code
               "(func main () (if (not 0) 20 40))")
              40)
           "not-int")

  (test/is (= (doeval-exit-code
               "(func main () (first (reverse (list 5 6 7))))")
              7)
           "reverse")

  (test/is (= (doeval-exit-code
               "(func main () (first (map inc (list 5 6 7))))")
              6)
           "map")

  (test/is (= (doeval-exit-code
               "(func tgt (x) (> x 6))
                (func main () (first (filter tgt (list 5 6 7))))")
              7)
           "filter")

  (test/is (= (doeval-exit-code
               "(func main () ((fn () 100)))")
              100)
           "simple anonymous function")

  (test/is (= (doeval-exit-code
               "(func main ()
                  (first
                    (filter
                      (fn (x) (> x 10))
                      (list 5 10 15))))")
              15)
           "fancy anonymous function")

  )



