(def first (fn (seq) (builtin :first seq)))
(def rest (fn (seq) (builtin :rest seq)))
(def cons (fn (x seq) (builtin :cons x seq)))
(def + (fn (a b) (builtin :add a b)))
(def = (fn (a b) (builtin :compare a b)))

(def sum (fn (seq)
           (let (_sum (fn foo (total remaining)
                        (if (= remaining nil)
                          total
                          (foo (+ total (first remaining)) (rest remaining)))))
             (_sum 0 seq))))
