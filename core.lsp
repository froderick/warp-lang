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

(def str (fn str (& args)
           (let* (_str (fn _str (strings remaining)
                        (if (empty? remaining)
                          (join (reverse strings))
                          (_str (cons (to-string (first remaining)) strings)
                                (rest remaining)))))
             (_str nil args))))

(def gensym-state 0)

(def gensym (fn gensym ()
              (let* (n gensym-state
                    _ (def gensym-state (inc n)))
                (symbol (str "gensym-" n)))))

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
    (or (eq t 0) (eq t 7))))

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
