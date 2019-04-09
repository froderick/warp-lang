(def first (fn first (seq) (builtin :first seq)))
(def rest (fn rest (seq) (builtin :rest seq)))
(def cons (fn cons (x seq) (builtin :cons x seq)))
(def + (fn + (a b) (builtin :add a b)))
(def - (fn - (a b) (builtin :subtract a b)))
(def = (fn = (a b) (builtin :compare a b)))

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


;; begin temporary concat
(def concat-two (fn concat-two (seq-a seq-b)
                    (if (nil? seq-a)
                        seq-b
                      (concat-two (rest seq-a) (cons (first seq-a) seq-b)))))

(def concat-n (fn concat-fn (concated remaining)
                  (if (nil? remaining)
                      concated
                    (let (next (first remaining)
                               todo (rest remaining))
                      (concat-n (concat-two (reverse concated) next) todo)))))

(def concat (fn concat (& seqs)
                (concat-n '() seqs)))
;; end temporary concat

(def defn (fn defn (args)
              (let (name (first args)
                    fnargs (second args)
                    forms (drop 2 args))
                `(def ~name (fn ~name ~fnargs ~@forms)))))
(builtin :setmacro "defn")

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

(defn and (a b)
  (if a
    (if b true false)
    false))

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
