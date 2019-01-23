(def first (fn (seq) (builtin :first seq)))
(def rest (fn (seq) (builtin :rest seq)))
(def cons (fn (x seq) (builtin :cons x seq)))
(def + (fn (a b) (builtin :add a b)))
(def - (fn (a b) (builtin :subtract a b)))
(def = (fn (a b) (builtin :compare a b)))

(def sum (fn (seq)
  (let (_sum (fn foo (total remaining)
            (if (= remaining nil)
              total
              (foo (+ total (first remaining)) (rest remaining)))))
    (_sum 0 seq))))


(def fib (fn (n)
             (let (_fib (fn _fib (prev1 prev2 n)
                            (if (= n 0)
                                prev2
                              (_fib prev2 (+ prev1 prev2) (- n 1)))))
               (_fib 0 1 n))))

