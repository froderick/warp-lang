;;
;; pre-macro primitives
;;

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

(def str (fn str (& args)
           (let (_str (fn _str (strings remaining)
                        (if (empty? remaining)
                          (join (reverse strings))
                          (_str (cons (print-str (first remaining)) strings)
                                (rest remaining)))))
             (_str nil args))))

(def gensym-state 0)

(def gensym (fn gensym ()
              (let (n gensym-state
                    _ (def gensym-state (inc n)))
                (symbol (str "gensym-" n)))))

; TODO: add auto-gensym support with macro-syntax for generating bindings with it to avoid lexical capture
; https://www.braveclojure.com/writing-macros/
(def defmacro (fn defmacro (name fnargs & forms)
                `(let '()
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
    (let (n (first seq)
          seq (rest seq)
          sym (gensym))
      (if (empty? seq)
        `(let (~sym ~n)
           (if ~sym true false))
        `(let (~sym ~n)
           (if ~sym
             (and ~@seq)
             false))))))

(defmacro or (& seq)
  (if (empty? seq)
    nil
    (let (n (first seq)
          seq (rest seq)
          sym (gensym))
      (if (empty? seq)
        `(let (~sym ~n) ~sym))
        `(let (~sym ~n)
           (if ~sym
             ~sym
             (or ~@seq))))))

(defn not (a)
  (if a false true))

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

(defn take (n coll)
  (let (_take (fn _take (accum n coll)
                 (if (or (zero? n) (nil? coll))
                   (reverse accum)
                   (_take (cons (first coll) accum)
                          (dec n)
                          (rest coll)))))
    (_take nil n coll)))

(defn count (seq)
  (let (_count (fn _count (i remaining)
                 (if (empty? remaining)
                   i
                   (_count (inc i) (rest remaining)))))
    (_count 0 seq)))

;; todo: throw exceptions on invalid input
(defmacro cond (& seq)
  (if (empty? seq)
    nil
    (let (test (first seq)
          expr (second seq)
          seq (drop 2 seq))
      `(if ~test
         ~expr
         (cond ~@seq)))))

;; todo: print-bytecode for vars and for arbitrary expressions, deref vars / @, macroexpand

(defmacro do (& forms)
  `(let '() ~@forms))

;;
;; used for testing
;;

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

(defn example ()
  (and (+ 1 2) (+ 3 'x)))

