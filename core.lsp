(def first (fn (seq) (builtin :first seq)))
(def rest (fn (seq) (builtin :rest seq)))
(def cons (fn (x seq) (builtin :cons x seq)))
(def + (fn (a b) (builtin :add a b)))
(def = (fn (a b) (builtin :compare a b)))

(def _sum (fn (total remaining)
  (if (= remaining nil)
    total
    (_sum (+ total (first remaining)) (rest remaining)))))

(def sum (fn (seq) (_sum 0 seq)))
