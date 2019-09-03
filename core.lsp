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

(def gensym-state 0)

(def gensym (fn gensym ()
              (let* (n gensym-state
                    _ (def gensym-state (inc n)))
                (symbol (join (list "gensym-" (uint-to-string n)))))))

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
    (or (eq t 'nil) (eq t 'list))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn interpose (sep coll)
  (if (empty? coll)
    coll
    (let loop (remaining coll
               done ())
      (let (next (first remaining)
            remaining (rest remaining)
            done (if (empty? remaining)
                   (cons next done)
                   (cons sep (cons next done))))
            (if (empty? remaining)
              (reverse done)
              (loop remaining done))))))

(defn pr-list (v)
  (if (empty? v)
    "()"
    (let loop (printed (list ")")
               remaining v)
      (let (printed (cons (pr (first remaining)) printed)
            remaining (rest remaining))
        (if (empty? remaining)
          (join (interpose " " (reverse (cons "(" printed)))))
          (loop printed remaining)))))

;; (let (x (pr-list (list 1 2 3))) x)

(defn pr (v)
  (let (type (get-type v))
    (cond

      (nil? type) (throw "type is nil")

      ;; atoms
      (eq type 'nil) "nil"
      (eq type 'uint) (uint-to-string v)
      (eq type 'bool) (if v "true" "false")
      (eq type 'char) (char-to-string v)
      (eq type 'string) v
      (eq type 'symbol) (-> v name)
      (eq type 'keyword) (join (list ":" (-> v name)))

      ;; collections
      (eq type 'list) (pr-list v)
;;       (eq type 'array)
;;       (eq type 'map)
;;       (eq type 'record)

      ;; un-printables
      (eq type 'closure) "<closure>"
      (eq type 'cfn) "<cfn>"
      (eq type 'fn) "<fn>"

      :else (throw-value "unhandled type" type))))

(defn str (& args)
  (let loop (strings nil
             remaining args)
    (if (empty? remaining)
      (join (reverse strings))
      (loop (cons (pr (first remaining)) strings)
            (rest remaining)))))

(defn string-hash (s)
  (let (len (count s))
    (if (zero? len)
      0
      (let loop (i 0
                 h 0)
        (if (< i len)
          (loop (inc i) (+ (* 31 h) (char-to-uint (get s i))))
          h)))))

(defn hash-code (v)
  (let (type (get-type v))
    (cond
      (eq type 'nil) 0
      (eq type 'uint) v
      (eq type 'bool) (if v 1 0)
      (eq type 'char) (char-to-uint v)
      (eq type 'string) (string-hash v)
      (eq type 'symbol) (-> v name string-hash)
      (eq type 'keyword) (-> v name string-hash)
      :else (throw-value (str "can't hash this type of value: " v)))))

