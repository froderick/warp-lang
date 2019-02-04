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
                (list 'def name (concat (list 'fn name fnargs) forms)))))
(builtin :setmacro "defn")
