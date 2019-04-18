(def nil? (fn nil? (x) (= nil x)))
(def zero? (fn zero? (n) (= n 0)))
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
                 (let (_reverse (fn _reverse (old new)
                                    (if (nil? old)
                                        new
                                      (let (n (first old)
                                              old (rest old))
                                        (_reverse old (cons n new))))))
                   (_reverse seq nil))))

(def concat (fn concat (& seqs)
              (let (concat-two (fn concat-two (seq-a seq-b)
                                 (if (nil? seq-a)
                                   seq-b
                                   (concat-two (rest seq-a) (cons (first seq-a) seq-b))))
                    concat-n (fn concat-n (concated remaining)
                               (if (nil? remaining)
                                 concated
                                 (let (next (first remaining)
                                       todo (rest remaining))
                                   (concat-n (concat-two (reverse concated) next) todo)))))
                (concat-n '() seqs))))

(def defmacro (fn defmacro (name fnargs & forms)
                `(let '()
                   (def ~name (fn ~name ~fnargs ~@forms))
                   (set-macro (quote ~name)))))
(set-macro 'defmacro)

(defmacro defn (name fnargs & forms)
  `(def ~name (fn ~name ~fnargs ~@forms)))

(defmacro do (& forms)
  `(let '() ~@forms))

(defn list? (x)
  (let (t (get-type x))
    (or (= t 0) (= t 7))))

(defmacro -> (x & exprs)
  (let (->helper (fn ->helper (x exprs)
                   (if (nil? exprs)
                     x
                     (let (expr (first exprs)
                           val (if (list? expr)
                                 `(~(first expr) ~x ~@(rest expr))
                                 (list expr x)))
                       (->helper val (rest exprs))))))
  (->helper x exprs)))

(defn prn (x) (prn x))

(defn fib (n)
  (let (_fib (fn _fib (prev1 prev2 n)
                 (if (= n 0)
                     prev2
                   (_fib prev2 (+ prev1 prev2) (- n 1)))))
    (_fib 0 1 n)))

(defn large (n)
  (let (_large (fn _large (n seq)
                  (if (zero? n)
                    seq
                    (_large (dec n) (cons n seq)))))
    (_large n nil)))

(defn empty? (seq)
  (nil? seq))

;; TODO: rewrite these boolean ops as macros
;; TODO: use gensym to avoid lexical capture
(defmacro and (& seq)
  (if (empty? seq)
    true
    (let (n (first seq)
          seq (rest seq))
      (if (empty? seq)
        `(let (a1 ~n)
           (if a1 true false))
        `(let (a1 ~n)
           (if a1
             (and ~@seq)
             false))))))

(defn example ()
  (and (+ 1 2) (+ 3 'x)))

(defn or (a b)
  (if a
    a
    (if b b nil)))

(defn not (a)
  (if a false true))

(defn take (n coll)
  (let (_take (fn _take (accum n coll)
                 (if (or (zero? n) (nil? coll))
                   (reverse accum)
                   (_take (cons (first coll) accum)
                          (dec n)
                          (rest coll)))))
    (_take nil n coll)))


;; todo: not, cond, print-bytecode for vars and for arbitrary expressions, deref vars / @


;; TODO: figure out how to do c interop, to implement things like string concatenation without creating new vm instructions
;;
;; The JNI way involves handing an Env reference as the first parameter to every native c function call.
;; The rest of the parameters correspond to normal function arguments, they are all Value types.
;;
;; Native functions can be modeled with a special type of stack frame and a special object type that we honor as invocable.
;; Invoking a special object type results in the special stack frame. Special objects must be constructed with a dedicated
;; vm instruction. There will be a registry of special objects that are automatically created and defined as procedures
;; on vm initialization.
;;
;; One job of the special stack is to hold references to all the parameters into a c function, so that they are not
;; garbage-collected prematurely. Garbage collection may happen in the middle of a c function, if the c function does
;; something to trigger it such as allocating memory.
;;
;; One of the things to think about is how to accomodate the GC mid-c-function. To ensure that local references to
;; objects survive a GC relocation, we must do like JNI and keep a special reference to the value that isn't the actual
;; value reference itself. This way, c code does not have to be aware of relocations of objects. This also means that
;; c code cannot have direct access to the memory representation of objects. It must all be proxied through the
;; Environment.
;;
;; The environment must be a composite of the actual VM and the specific special stack frame that contains the registry
;; of the object references the c call holds. When a c call obtains a new reference, this registry should be kept
;; up to date. The c call should be able to voluntarily free references it obtains.
;;
;; There may be a need for a feature that copies the entire contents of an object into non-relocatable memory
;; so that a c function can operate on it safely without worrying about relocation.
;;
;; Another question is how would the VM invoke a special function object? Once the special stack frame is set up,
;; how does the actual c code get called? There is a level of dynamic functionality here that requires knowlege of
;; the c calling conventions. Something has to know how to set up the parameters and jump to the function in question.
;; Does this need to be written in assembly?

;;
;; I thought more about this, and concluded that I don't really need a general purpose FFI like RMI. I can get away
;; with a basic mechanism to invoke c functions where each c function has to be aware of gc and explicitly register
;; references it wants to hold

;; TODO: support coercing values to strings, starting with numbers (str)
;; TODO: support concatenating strings
;; TODO: make gensym, and the macro-syntax support for generating let-bindings with it to avoid lexical capture
;(def gensym-state 0)
;
;(defn gensym ()
;  (let (n gensym-state
;        _ (def gensym-state (inc n)))
;    ;; concat a fixed-string 'gensym-' with n, a number
;    ;; make a symbol from the concated string
;  ))


