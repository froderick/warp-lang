;;
;; pre-macro primitives
;;

(def nil? (fn nil? (x) (eq nil x)))
(def zero? (fn zero? (n) (eq n 0)))
(def second (fn second (seq) (first (rest seq))))
(def inc (fn inc (n) (+ n 1)))
(def dec (fn dec (n) (- n 1)))

(def nth (fn nth (i seq)
             (if (zero? i)
                 (first seq)
                 (nth (dec i) (rest seq)))))

(def drop (fn drop (i seq)
             (if (zero? i)
                 seq
               (drop (dec i) (rest seq)))))

(def reverse (fn reverse (seq)
                 (let* (_reverse (fn _reverse (old new)
                                    (if (nil? old)
                                        new
                                      (let* (n (first old)
                                              old (rest old))
                                        (_reverse old (cons n new))))))
                   (_reverse seq nil))))

(def concat (fn concat (& seqs)
              (let* (concat-two (fn concat-two (seq-a seq-b)
                                 (if (nil? seq-a)
                                   seq-b
                                   (concat-two (rest seq-a) (cons (first seq-a) seq-b))))
                    concat-n (fn concat-n (concated remaining)
                               (if (nil? remaining)
                                 concated
                                 (let* (next (first remaining)
                                       todo (rest remaining))
                                   (concat-n (concat-two (reverse concated) next) todo)))))
                (concat-n '() seqs))))

(def gensym-state 0)

(def gensym (fn gensym ()
              (let* (n gensym-state
                    _ (def gensym-state (inc n)))
                (symbol (join (list "gensym-" (uint-to-string n)))))))

; TODO: add auto-gensym support with macro-syntax for generating bindings with it to avoid lexical capture
; https://www.braveclojure.com/writing-macros/
(def defmacro (fn defmacro (name fnargs & forms)
                `(let* '()
                   (def ~name (fn ~name ~fnargs ~@forms))
                   (set-macro (quote ~name)))))
(set-macro 'defmacro)

;;
;; post-macro standard library, general reliance on macros after this point
;;

(defmacro defn (name fnargs & forms)
  `(def ~name (fn ~name ~fnargs ~@forms)))

(defn empty? (seq)
  (nil? seq))

(defmacro and (& seq)
  (if (empty? seq)
    true
    (let* (n (first seq)
          seq (rest seq)
          sym (gensym))
      (if (empty? seq)
        `(let* (~sym ~n)
           (if ~sym true false))
        `(let* (~sym ~n)
           (if ~sym
             (and ~@seq)
             false))))))

(defmacro or (& seq)
  (if (empty? seq)
    nil
    (let* (n (first seq)
          seq (rest seq)
          sym (gensym))
      (if (empty? seq)
        `(let* (~sym ~n) ~sym))
        `(let* (~sym ~n)
           (if ~sym
             ~sym
             (or ~@seq))))))

;;(defmacro list (& seq)
;;  (if (empty? seq)
;;    nil
;;    (let* (n (first seq)
;;          seq (rest seq))
;;      (if (empty? seq)
;;        `(cons ~n nil)
;;        `(cons ~n (list ~@seq))))))

(defn not (a)
  (if a false true))

(defn list? (x)
  (let* (t (get-type x))
    (or (eq t 'nil) (eq t 'list))))

;; todo: throw exceptions on invalid input
(defmacro cond (& seq)
  (if (empty? seq)
    nil
    (let* (test (first seq)
           expr (second seq)
           seq (drop 2 seq))
      `(if ~test
         ~expr
         (cond ~@seq)))))

(defn map (f coll)
  (let* (_map (fn _map (old new)
                 (if (empty? old)
                     new
                     (_map (rest old) (cons (f (first old)) new)))))

    (reverse (_map coll (list)))))

(defn partition (coll)
  (let* (_partition (fn _partition (old new)
                      (cond
                        (empty? old) (reverse new)
                        (eq (count old) 1) (throw "requires a list with an even number of items")
                        :else (_partition (drop 2 old) (cons (list (first old) (second old)) new)))))
    (_partition coll '())))

(defmacro let (& forms)
  (if (symbol? (first forms))
    (let* (fn-name (first forms)
           bindings (second forms)
           forms (rest forms)
           arg-parts (partition bindings)
           arg-names (map first arg-parts)
           arg-values (map second arg-parts))
      `(let* (~fn-name (fn ~fn-name ~arg-names ~@(rest forms)))
         (~fn-name ~@arg-values)))
      `(let* ~@forms)))

(defmacro -> (x & exprs)
  (let loop (x x
             exprs exprs)
    (if (nil? exprs)
      x
      (let* (expr (first exprs)
            val (if (list? expr)
                  `(~(first expr) ~x ~@(rest expr))
                  (list expr x)))
        (loop val (rest exprs))))))

(defn = (x y)
  (if (and (list? x) (list? y))
    (let loop (x* x y* y)
      (cond
        (and (empty? x*) (empty? y*)) true
        (and (empty? x*)) false
        (and (empty? y*)) false
        (not (= (first x*) (first y*))) false
        :else (loop (rest x*) (rest y*))))
    (eq x y)))

;; todo: print-bytecode for vars and for arbitrary expressions, deref vars / @, macroexpand

(defmacro do (& forms)
  `(let* '() ~@forms))

(defn last (x)
  (let* (remainder (rest x))
    (if (nil? remainder)
      (first x)
      (last (rest x)))))

(defn take (n coll)
  (let loop (accum nil
        n n
        coll coll)
    (if (or (zero? n) (nil? coll))
      (reverse accum)
      (loop (cons (first coll) accum)
            (dec n)
            (rest coll)))))

(defn butlast (x)
  (let (n (count x))
    (cond
      (zero? n) n
      (eq 1 n) '()
      :else (take (dec n) x))))

(defmacro try (& forms)
  (let* (catch (last forms))

    (if (not (= (first catch) 'catch))
      (throw "the last form in a try must be a catch"))

    (if (zero? (count (rest catch)))
      (throw "a catch clause must include the name of the exception binding"))

    (let* (e-binding (nth 1 catch)
          catch-forms (drop 2 catch))
      `(with-handler (fn (~e-binding) ~@catch-forms)
         ~@(butlast forms)))))

;;
;; used for testing
;;

(defn fib (n)
  (let* (_fib (fn _fib (prev1 prev2 n)
                 (if (= n 0)
                     prev2
                   (_fib prev2 (+ prev1 prev2) (- n 1)))))
    (_fib 0 1 n)))

(defn fib2 (start)
  (let loop (prev1 0
             prev2 1
             n start)
    (if (= n 0)
      prev2
      (loop prev2 (+ prev1 prev2) (- n 1)))))

(defn large (n)
  (let* (_large (fn _large (n seq)
                  (if (zero? n)
                    seq
                    (_large (dec n) (cons n seq)))))
    (_large n nil)))

(defn example ()
  (and (+ 1 2) (+ 3 'x)))

(defn split (coll)
  (let loop (old coll
             coll-a '()
             coll-b '())
    (cond
      (empty? old) (list (reverse coll-a) (reverse coll-b))
      (= (count old) 1) (throw "requires a list with an even number of items")
      :else (loop
              (drop 2 old)
              (cons (first old) coll-a)
              (cons (second old) coll-b)))))

;; (defn count (seq)
;;   (let* (_count (fn _count (i remaining)
;;                  (if (empty? remaining)
;;                    i
;;                    (_count (inc i) (rest remaining)))))
;;     (_count 0 seq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn interpose (sep coll)
  (if (empty? coll)
    coll
    (let loop (remaining coll
               done ())
      (let (next (first remaining)
            remaining (rest remaining)
            done (if (empty? remaining)
                   (cons next done)
                   (cons sep (cons next done))))
            (if (empty? remaining)
              (reverse done)
              (loop remaining done))))))

(defn pr-quote-string (v)
  (if (eq (get-type v) 'string)
    (join (list "\"" v "\""))
    v))

(defn pr-list (v)
  (if (empty? v)
    "()"
    (let loop (printed '()
               remaining v)
      (if (empty? remaining)
        (join (cons "(" (reverse (cons ")" (interpose " " printed))))) ;; TODO: finish thread-last
        (loop
          (cons (pr-str (first remaining)) printed)
          (rest remaining))))))

; (pr-list (list 1 2 3))

(defn pr-array (v)
  (if (zero? (count v))
    "[]"
    (let loop (printed '()
               i 0)
      (if (= i (count v))
        (join (cons "[" (reverse (cons "]" (interpose " " printed))))) ;; TODO: finish thread-last
        (loop
          (cons (pr-str (get v i)) printed)
          (inc i))))))

(defn pr-str (v)
  (let (type (get-type v))
    (cond

      ;; atoms
      (nil? v) "nil"
      (eq type 'uint) (uint-to-string v)
      (eq type 'bool) (if v "true" "false")
      (eq type 'char) (join (list "'" (char-to-string v) "'"))
      (eq type 'string) (pr-quote-string v)
      (eq type 'symbol) (-> v name)
      (eq type 'keyword) (join (list ":" (-> v name)))

      ;; collections
      (eq type 'list) (pr-list v)
      (eq type 'array) (pr-array v)
      (eq type 'map) "<map>"
      (eq type 'record) (pr-record v)

      ;; un-printables
      (eq type 'closure) "<closure>"
      (eq type 'cfn) "<cfn>"
      (eq type 'fn) "<fn>"

      :else (throw-value "unhandled type" type))))

(defn str-one (v)
  (let (type (get-type v))
    (cond

      ;; atoms
      (nil? v) "nil"
      (eq type 'uint) (uint-to-string v)
      (eq type 'bool) (if v "true" "false")
      (eq type 'char) (char-to-string v)
      (eq type 'string) v
      (eq type 'symbol) (-> v name)
      (eq type 'keyword) (join (list ":" (-> v name)))

      :else (pr-str v))))

(defn str (& args)
  (let loop (strings nil
             remaining args)
    (if (empty? remaining)
      (join (reverse strings))
      (loop (cons (str-one (first remaining)) strings)
            (rest remaining)))))

(defn string-hash (s)
  (let (len (count s))
    (if (zero? len)
      0
      (let loop (i 0
                 h 0)
        (if (< i len)
          (loop (inc i) (+ (* 31 h) (char-to-uint (get s i))))
          h)))))

(defn hash-code (v)
  (let (type (get-type v))
    (cond
      (nil? v) 0
      (eq type 'uint) v
      (eq type 'bool) (if v 1 0)
      (eq type 'char) (char-to-uint v)
      (eq type 'string) (string-hash v)
      (eq type 'symbol) (-> v name string-hash)
      (eq type 'keyword) (-> v name string-hash)
      :else (throw-value (str "can't hash this type of value: " type) v))))

; (defrecord hi (one two three))
; (def x (make-hi :x :y :z))
; (-> (make-hi :x :y :z) hi-one)

(defn list->vector (l)
  (let (v (make-vector (count l)))
    (let loop (i 0
               remaining l)
      (if (empty? remaining)
        v
        (do
          (set v i (first remaining))
          (loop (inc i) (rest remaining)))))))

(defn make-record-kw-accessor (name fields)
  (let (num-fields (count fields)
        kw-sym (gensym))
    (let loop (i 0
               cond-args '())
      (if (eq i num-fields)
       `(fn ~(symbol (str name "-kw-get")) (obj ~kw-sym)
          (cond
             ~@cond-args
             :else nil))
        (loop
          (inc i)
          (concat cond-args `((eq ~kw-sym ~(get fields i)) (get obj ~i))))))))

(defn make-record-constructor (rname rfields)

  (let (init (let loop (i 0
                        cmds '()
                        remaining rfields)
               (if (empty? remaining)
                 (reverse cmds)
                 (let (cmd `(set r ~i ~(first remaining)))
                   (loop (inc i)
                         (cons cmd cmds)
                         (rest remaining))))))
    (let (vec-fields (-> (map keyword rfields) list->vector))

      `(defn ~(symbol (str "make-" (name rname))) ~rfields
         (let (r (record [(quote ~rname)
                          (quote ~vec-fields)
                          ~(make-record-kw-accessor rname vec-fields)]
                         ~(count rfields)))
           ~@init
           r)))))

(defn record-name (r)
  (-> r record-type (get 0)))

(defn record-fields (r)
  (-> r record-type (get 1)))

(defn record-kw-accessor (r)
  (-> r record-type (get 2)))

(defn make-record-pred (rname)
  `(defn ~(symbol (str (name rname) "?")) (obj)
     (eq (quote ~rname) (-> obj record-type (get 0)))))

(defn make-record-accessor (rname rfield idx)
  `(defn ~(symbol (str (name rname) "-" (name rfield))) (p)
     (get p ~idx)))

(defn make-record-accessors (rname rfields)
  (let loop (i 0
             cmds '()
             remaining rfields)
    (if (empty? remaining)
      (reverse cmds)
        (loop (inc i)
              (cons (make-record-accessor rname (first remaining) i) cmds)
              (rest remaining)))))

(defn make-record-mutator (rname rfield idx)
  `(defn ~(symbol (str "set-" (name rname) "-" (name rfield) "!")) (p obj)
     (set p ~idx obj)))

(defn make-record-mutators (rname rfields)
  (let loop (i 0
             cmds '()
             remaining rfields)
    (if (empty? remaining)
      (reverse cmds)
        (loop (inc i)
              (cons (make-record-mutator rname (first remaining) i) cmds)
              (rest remaining)))))

(defmacro defrecord (& forms)
  (let (rname (first forms)
        rfields (second forms))

    `(do
       ~(make-record-constructor rname rfields)
       ~(make-record-pred rname)
       ~@(make-record-accessors rname rfields)
       ~@(make-record-mutators rname rfields)
       )))

(defn pr-record (r)
  (let (fields (record-fields r)
        num-fields (count fields))
    (let loop (i 0
               field-values '())
      (if (eq i num-fields)
        (str "#" (name (record-name r)) "{" (join (interpose " " (reverse field-values))) "}")
        (loop
          (inc i)
          (cons (pr-str (get r i)) ;; TODO: need thread-last
                (cons (pr-str (get fields i))
                      field-values)))))))

(defn equals-str? (a b)
  (if (not (eq (count a) (count b)))
    false
    (let loop (i 0)
      (if (eq i (count a))
        true
        (if (eq (get a i) (get b i))
          (loop (inc i))
          false)))))

(defn equals? (a b)
  (let (type (get-type a))
    (cond
      (eq type 'string) (equals-str? a b)
      :else (eq a b))))

; (defrecord hi (one two three))
; (def x (make-hi "One" "Two" "Three"))
; (pr-str x)
; ((record-kw-accessor x) x :one)

(defrecord map (size entries))
(defrecord map-entry (key hash value))

(defn create-map (& args)
  (make-map 0 (make-vector 16)))

(defn find-map-entry-index (m key hash)
  (let (entries (map-entries m)
        idx-search (fn idx-search (start stop)
                     (let loop (i start)
                       (cond
                         (eq i stop) nil ;; entry not found
                         (let (e (get entries i))
                           (or (nil? e) (equals? key (map-entry-key e)))) i ;; entry found
                         :else (loop (inc i)))))
        index (mod hash (count entries))
        entry-idx (idx-search index (count entries)))
    (if entry-idx
      entry-idx
      (idx-search 0 index))))

;; (defn keyword? (obj)
;;   (eq (get-type obj) 'keyword))
;;
;;  (defn find-map-entry-index (m key hash)
;;    (let (entries (map-entries m)
;;          idx-search (fn idx-search (start stop)
;;
;;                       ;(if (keyword? entries)
;;                       ;  (throw-value "wrong value in entries" entries))
;;
;;                       (let looper (i start)
;;                         (cond
;;                           (eq i stop) nil ;; entry not found
;;                           (let (e (get entries i))
;;                             (or (nil? e) (equals? key (map-entry-key e)))) i ;; entry found
;;                           :else (looper (inc i)))))
;;          index (mod hash (count entries))
;;          entry-idx (idx-search index (count entries)))
;;      (if entry-idx
;;        entry-idx
;;        (idx-search 0 index))))

;; update:
;; - the `3:	I_LOAD_LOCAL	3` in idx-search is wrong, it should be 2. when I edit it in the debugger to the right
;;   value, this function appears to work just fine.

(find-map-entry-index (create-map) :a (hash-code :a))


;; seems to be going wrong loading the keyword in `21:	I_LOAD_LOCAL	2` rather than the vector in
;; I actually can't find the captured `entries` vector anywhere in the locals for this function

(defn test (a b)
  (let (c :c
        cl (fn cl (d e)
             (let loop (f e)
               (if (not (eq c :c))
                 (throw-value "wrong value c" c)))))
    (cl :d :e)))












