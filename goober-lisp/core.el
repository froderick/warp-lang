(defmacro defn (name args & rest)
  (let (f (cons 'fn (cons args rest)))
   (list 'def name f)))

(defn not (x) (if x false true))

(defmacro when (test & rest) 
  (list 'if test (cons 'do rest)))

(defn empty? (coll) (= (count coll) 0))

(defn second (coll) (first (rest coll)))

(defn inc (n) (+ n 1))
(defn dec (n) (- n 1))

(defn reverse (coll)
  (let (map-inner (fn (old-coll new-coll)
                    (if (empty? old-coll)
                      new-coll
                      (recur
                        (rest old-coll)
                        (cons (first old-coll) new-coll)))))
    (map-inner coll (list))))

(defn map (f coll)
  (let (map-inner (fn (old-coll new-coll)
                    (if (empty? old-coll)
                      new-coll
                      (recur
                        (rest old-coll)
                        (cons (f (first old-coll)) new-coll))))
        mapped (map-inner (seq coll) (list)))
    (reverse mapped)))

(defmacro apply (f & rest)
  (cons f rest))

(defn and (& args)
  (if (empty? args)
    true
    (if (first args)
        (apply and (rest args))
        false)))

(defn fib (n)
  (let (fib-n (fn (x n)
                (if (< (count x) n) 
                  (recur (cons (+ (first x) (second x)) x) n)
                  (reverse x))))
    (fib-n '(1 0) n)))

(defn describe-person (name & other-stuff)
  (println "name:" name)
  (println "other-stuff:" other-stuff))

