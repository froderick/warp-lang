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

(def second (fn (seq)
                (first (rest seq))))

(def zero? (fn (n) (= n 0)))

(def inc (fn (n) (+ n 1)))
(def dec (fn (n) (- n 1)))

(def nth (fn loop (i seq)
             (if (zero? i)
                 (first seq)
                 (loop (dec i) (rest seq)))))

(def drop (fn loop (i seq)
             (if (zero? i)
                 seq
               (loop (dec i) (rest seq)))))

(def nil? (fn (x) (= nil x)))

(def reverse (fn (seq)
                 (let (_reverse (fn loop (old new)
                                    (if (nil? old)
                                        new
                                      (let (n (first old)
                                              old (rest old))
                                        (loop old (cons n new))))))
                   (_reverse seq nil))))

(def concat (fn (seq-a seq-b)
                (let (_concat (fn loop (seq-a seq-b)
                                  (if (nil? seq-a)
                                      seq-b
                                      (loop (rest seq-a) (cons (first seq-a) seq-b)))))
                  (_concat (reverse seq-a) seq-b))))

(def adder (fn (args)
               (cons '+
                     (cons (first args)
                           (cons (second args) nil)))))
(builtin :setmacro "adder")

(def defn (fn (args)
              (let (name (first args)
                    fnargs (second args)
                    forms (drop 2 args)
                    fnlist (cons 'fn
                                 (cons name
                                       (cons fnargs nil)))
                    fullfn (concat fnlist forms))
                (cons 'def 
                      (cons name
                            (cons fullfn nil))))))
(builtin :setmacro "defn")

;(def list (fn (args) args))
;(builtin :setmacro "list")

;;(def when (fn (args)
;;              (let ())
;;              (concat 
;;
;;               (cons 'if (cons (first args) nil)
;;                           )
;;                          ()
;;              (let (name (first args)
;;                         fnargs (second args)
;;                         forms (drop 2 args)
;;                         fnlist (cons 'fn
;;                                      (cons name
;;                                            (cons fnargs nil)))
;;                         fullfn (concat fnlist forms))
;;                (cons 'def 
;;                      (cons name
;;                            (cons fullfn nil))))))
;;(builtin :setmacro "defn")
                

;;(defn name (args) forms)
;;
;;(def defn (fn (args)
;;              (let (name (first args)
;;                    fnargs (second args)
;;                    forms (drop 2 args))
;;                `(def ~name (fn ~name ~args ~@forms))))))
;;(builtin :setmacro "defn")
