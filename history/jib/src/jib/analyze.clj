(ns jib.analyze
  "Currently contains both the parser and analyzer for the jib language."
  (:require [clojure.spec.alpha :as s]
            [clj-antlr.core :as antlr]
            [clojure.pprint :refer [pprint]]
            [clojure.walk :as walk]
            [clojure.zip :as zip]
            [clojure.test :as test]

            [jib.il :as il]))

(defn fail [& msgs]
  (throw (Exception. (apply str "analysis failed: " msgs))))

;; grammar and parsing code ;;;;;;;;;;;;;;;;;;;

(def parser
  (antlr/parser
   "grammar jib;

    exprs : expr*;

    expr : atom | sexpr | reader_macro;

    sexpr : O_PAREN C_PAREN | O_PAREN expr+ C_PAREN;

    atom : nil | boolean | symbol | keyword | string | number;
    nil : NIL;
    boolean : BOOLEAN;
    keyword : ':' symbol;
    symbol : SYMBOL;
    string : STRING;

    number : NUMBER;

    reader_macro : quote;
    quote : QUOTE expr;

    O_PAREN : '(';
    C_PAREN : ')';
    NIL : 'nil';
    BOOLEAN: 'true' | 'false';
    SYMBOL : [a-zA-Z_+/=!%$*^><?&]+ [a-zA-Z_+/=!%$*^><?&-]*
           | '-'
           | '+'
           | [+|-][a-zA-Z_-][0-9a-zA-Z_-] *;

    STRING : '\"' (~'\"')* '\"';

    NUMBER : [0-9]+ | '+' [0-9]+ | '-' [0-9]+;
    QUOTE : '\\'';

    COMMENT : ';' ~('\\r' | '\\n')* -> skip;
    WS : [ \\t\\r\\n]+ -> skip ;"))

(defn parse [text]
  {:text text
   :ast (antlr/parse parser text)})

;; ast analyzer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn strip-quotes
  "Assumes a string starts and ends with a single quote, and removes both."
  [^String s]
  (.substring s 1 (int (dec (count s)))))

(defn clean-atom [type value]
  (case type
    :string (strip-quotes value)
    value))

(defn mapify-ast
  "Make the ast less of a pain to navigate."
  [ast]
  (walk/postwalk
    (fn [f]
      (case (when (seq? f) (first f))
        :exprs (rest f)
        :expr (second f)
        :atom (let [[type value] (second f)]
                {:class :atom :type type :value (clean-atom type value)})
        :sexpr {:class :sexpr :value (filter map? f)}
        f))
    ast))

(defn parse-func
  "Parses an expression with the assumption that it defines a function.."
  [func]
  (let [[func-name raw-args & remaining] (-> func :value rest)]
    [(:value func-name)
     (map :value (:value raw-args))
     remaining]))

(defn parse-anonymous-func
  "Parses an expression with the assumption that it defines an anonymous function."
  [func]
  (let [[raw-args & remaining] (-> func :value rest)]
    [(map :value (:value raw-args))
     remaining]))

(defn parse-if
  "Parses an expression with the assumption that it defines a conditional branch.."
  [expr]
  (let [[test-expr a-expr b-expr] (-> expr :value rest)]
    [test-expr a-expr b-expr]))

(defn parse-let
  "Parses an expression with the assumption that it defines a let binding.."
  [expr]
  (let [args (-> expr :value rest)
        raw-bindings (-> args first :value)
        bindings (->> raw-bindings
                      (partition 2)
                      (map (fn [[k v]]
                             [(:value k) v])))
        exprs (rest args)]
    (when-not (zero? (mod (count raw-bindings) 2))
      (fail "bindings must be a list of even length: " (count raw-bindings)))
    [bindings exprs]))

(defn parse-loop
  [expr]
  (let [args (-> expr :value rest)
        raw-bindings (-> args first :value)
        bindings (->> raw-bindings
                      (partition 2)
                      (map (fn [[k v]]
                             [(:value k) v])))
        exprs (rest args)]
    (when-not (zero? (mod (count raw-bindings) 2))
      (fail "bindings must be a list of even length: " (count raw-bindings)))
    [bindings exprs]))

(defn parse-recur
  [expr]
  (-> expr :value rest)) 

(defn parse-builtin
  [expr]
  (let [params (-> expr :value rest)
        builtin-name (-> params first :value)
        exprs (rest params)]
    [builtin-name exprs]))

(declare analyze-expr)
(declare analyze-exprs)

(defn analyze-if
  [ctx expr]

  (let [[test-expr a-expr b-expr] (parse-if expr)]
    (-> {:form :if
         :test (analyze-expr ctx test-expr)
         :if-branch (analyze-expr ctx a-expr)}
        (#(if b-expr
            (assoc % :else-branch (analyze-expr ctx b-expr)) %)))))

(defn analyze-exprs-with-tail-position [ctx exprs]
  (map-indexed (fn [idx expr]
                 (let [tail-idx (-> exprs count dec)
                       ctx (assoc ctx :tail-position? (= idx tail-idx))]
                   (analyze-expr ctx expr)))
               exprs))

(defn analyze-let
  [{:keys [bindings] :as ctx} expr]

  (let [[local-binding-pairs exprs] (parse-let expr)
        local-bindings (map (fn [[name expr]]
                              {:name name
                               :id (gensym "local")
                               :expr (analyze-expr ctx expr)})
                            local-binding-pairs)
        new-bindings (concat
                      (->> local-bindings
                           (map #(select-keys % [:name :id]))
                           reverse)
                      bindings)
        new-ctx (assoc ctx :bindings new-bindings)]

    {:form :let
     :bindings local-bindings
     :exprs (analyze-exprs-with-tail-position new-ctx exprs)}))

(defn analyze-loop
  [{:keys [bindings] :as ctx} expr]

  (let [[local-binding-pairs exprs] (parse-let expr)
        local-bindings (map (fn [[name expr]]
                              {:name name
                               :id (gensym "local")
                               :expr (analyze-expr ctx expr)})
                            local-binding-pairs)
        new-bindings (concat
                      (->> local-bindings
                           (map #(select-keys % [:name :id]))
                           reverse)
                      bindings)

        loop-start-label (gensym "Loopstart")

        new-ctx (assoc ctx :bindings new-bindings
                           :loop-start-label loop-start-label
                           :loop-bindings (map :id local-bindings))]
    {:form :loop
     :bindings local-bindings
     :exprs (analyze-exprs-with-tail-position new-ctx exprs)
     :loop-start-label loop-start-label}))

(defn analyze-recur
  [{:keys [loop-start-label loop-bindings tail-position?] :as ctx} expr]

  (let [exprs (parse-recur expr)]

    (when-not tail-position?
      (fail "recur not in tail position: " ctx exprs))

    (when-not loop-start-label
      (fail "recur found outside a loop: " ctx exprs))

    (let [required (count loop-bindings)
          supplied (count exprs)]
      (when-not (= required supplied)
        (fail "recur requires " required " arguments here, and " supplied " are supplied"))) 

    {:form :recur
     :exprs (map #(analyze-expr ctx %) exprs)
     :loop-start-label loop-start-label
     :loop-bindings loop-bindings}))

(defn find-binding
  [bindings name]
  (->> bindings
       (filter #(= (:name %) name))
       first))

(defn analyze-binding-ref
  [bindings binding-ref]
  (let [name (:value binding-ref)
        binding-id (->> name
                        (find-binding bindings)
                        :id)]
    (if-not (nil? binding-id)
      {:form :binding-ref
       :name name
       :binding-id binding-id}
      {:form :fn-callable
       :name name})))

(defn not-tail-position [ctx]
  (assoc ctx :tail-position? false))

(defn analyze-builtin [ctx expr]
  (let [[builtin-name exprs] (parse-builtin expr)
        ctx (not-tail-position ctx)]
    {:form :builtin
     :name builtin-name
     :args (map #(analyze-expr ctx %) exprs)}))

(defn analyze-fn-call
  [{:keys [bindings] :as ctx} {:keys [form name gen-func] :as fn-expr} raw-args]

  (merge

   {:form :fn-call
    :args (map #(analyze-expr (not-tail-position ctx) %) raw-args)
    :dynamic? false}

   (when gen-func {:gen-func gen-func})

   (case form

     :fn-callable
     {:name name}

     :binding-ref
     (if-let [fn-binding (find-binding bindings name)]
       {:dynamic? true :binding fn-binding}
       (fail "cannot find fn binding for " fn-expr))

     (fail "cannot identify the function to be called: " fn-expr))))

(defn analyze-func-def-args
  [args]
  (map
   (fn [arg] {:name arg :id (gensym "arg")})
   args))

(defn amp?
  [amp]
  (= "&" amp))

(defn is-variadic?
  [args]
  (let [num-amps (->> args (filter amp?) count)]
    (case num-amps
      0 false
      1 (let [[amp _] (drop (- (count args) 2) args)]
          (when-not (amp? amp)
            (fail "amp must be second-last argument in list: " amp))
          true)
      (fail "cannot have more than one amp in argument list: " num-amps))))

(defn apply-variadic
  [args]
  (if (is-variadic? args)
    [true (filter (complement amp?) args)]
    [false args]))

(defn analyze-func

  ([func]
   (let [[func-name args exprs] (parse-func func)]
     (analyze-func func-name args exprs)))

  ([func-name args exprs]
   (let [[variadic? args] (apply-variadic args)
         args (analyze-func-def-args args)
         bindings (reverse args)
         ctx {:bindings bindings}
         exprs (analyze-exprs-with-tail-position ctx exprs)
         entry-point? (= "-main" func-name)]

     {:form :func
      :name func-name
      :args args
      :entry-point? entry-point?
      :variadic? variadic?
      :exprs exprs})
   ))

(defn analyze-anonymous-func [{:keys [bindings] :as ctx} expr]
  (let [fn-name (name (gensym "anon_fn_"))
        [args exprs] (parse-anonymous-func expr)]
    {:form :fn-callable
     :name fn-name
     :gen-func (analyze-func fn-name args exprs)}))

(defn analyze-sexpr
  [ctx expr]

  (if (empty? (:value expr))
    (analyze-fn-call ctx {:form :fn-callable :name "list"} nil)

    (let [[fn-expr & args] (:value expr)
          {:keys [form id] :as fn-expr} (analyze-expr ctx fn-expr)]
      (if (= form :special)
        (case id
          :if      (analyze-if ctx expr)
          :let     (analyze-let ctx expr)
          :loop    (analyze-loop ctx expr)
          :recur   (analyze-recur ctx expr)
          :builtin (analyze-builtin ctx expr)
          :func    (analyze-func expr)
          :fn      (analyze-anonymous-func ctx expr)
          (fail "cannot identify the special form to be called: " fn-expr))
        (analyze-fn-call ctx fn-expr args)))))

(def special-forms #{"if" "let" "loop" "recur" "builtin" "func" "fn"})

(defn analyze-expr

  ([expr]
   (analyze-expr nil expr))

  ([{:keys [bindings] :as ctx} expr]
  (case (:class expr)

    :sexpr (analyze-sexpr ctx expr)

    :atom (case (:type expr)
            :number {:form :constant
                     :type :number
                     :value (-> expr :value Integer/parseInt)
                     :constant-id (gensym "constant")}
            :nil {:form :constant
                  :type :nil
                  :value nil
                  :constant-id (gensym "constant")}
            :boolean {:form :constant
                      :type :boolean
                      :value (-> expr :value Boolean/parseBoolean)
                      :constant-id (gensym "constant")}
            :string {:form :constant
                      :type :string
                     :value (-> expr :value)
                            #_(let [raw (-> expr :value)]
                              (.substring raw 1 (int (dec (count raw)))))
                      :constant-id (gensym "constant")}
            :symbol (if (contains? special-forms (:value expr))
                      {:form :special
                       :id (keyword (:value expr))}
                      (analyze-binding-ref bindings expr))
            (fail "only numbers supported as values: " expr))

    (fail "not a recognized expr type: " expr))))

(comment

  (try
    (->>   #_"(func main (y)
              (println y)
              (fn (x) (println x))
              )"
           "(func main () ((fn () 100)))"
           (antlr/parse parser)
           mapify-ast
           first
           analyze-expr
           pprint)
    (catch clj_antlr.ParseError e (pprint @e)))

)
