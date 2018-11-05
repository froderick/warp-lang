(ns interp.core
  (:require
   [clojure.pprint :refer [pprint]]))


(defn make-heap-object
  []
  {:color :white} ;; white gray black
  )

(defn process-raw-instructions
  [vm raw-instructions]
  (let [[vm _] (reduce
                (fn [[vm addr] x]
                  (if (symbol? x)
                    [(assoc-in vm [:aliases x] addr) addr]
                    [(update-in vm [:instructions] conj x) (inc addr)]))
                [vm 0]
                raw-instructions)]
    vm))

(defn make-vm
  [raw-instructions]

  (let [vm {:vars {}
            :frames []
            :heap []
            :stack []
            :instructions []
            :aliases {}
            :ip 0}
        vm (process-raw-instructions vm raw-instructions)
        main (-> vm
                 :aliases
                 (get 'main))]

    (when-not main
      (throw (Exception. (format "no main symbol to use for an entry point"))))

    (assoc vm :ip main)))

(defn log [{:keys [ip stack] :as vm} msg]
  (printf "[vm; ip=%s; stack=%s] %s\n" ip stack msg)
  nil)

(defn current-frame [{:keys [frames] :as vm}]
  (if-let [frame (peek frames)]
    frame
    (throw (Exception. (format "no current stack frame")))))

(defn current-stack-value [{:keys [stack] :as vm}]
  (if-let [val (peek stack)]
    val
    (throw (Exception. (format "no current stack value, stack is empty")))))

(defn get-addr
  [{:keys [aliases] :as vm} [ref-type ref-arg]]
  (case ref-type
    :address (if (integer? ref-arg)
               ref-arg
               (throw (Exception. "address arg must be an integer")))
    :alias (let [addr (get aliases ref-arg)]
             (when-not addr
               (throw (Exception. (format "no such alias found: %s" ref-arg))))
             addr)))

(defn inc-ip
  [vm]
  (update-in vm [:ip] inc))

(defn vm-push
  [{:keys [stack frames] :as vm} [ref-type ref-arg :as args]]
  (let [val (case ref-type

              :args  ;; relative function argument index
              (let [{:keys [saved-stack]} (current-frame vm)]
                (when-not (integer? ref-arg)
                  (throw (Exception. (format "arg-index must be an integer" args))))
                (nth saved-stack ref-arg))

              :local ;; relative local index
              (let [{:keys [locals] :as frame} (current-frame vm)]
                (when-not (integer? ref-arg)
                  (throw (Exception. (format "local-index must be an integer" args))))
                (get locals ref-arg))

              :const ;; constant value
              ref-arg)]

    (log vm (format "push %s %s" ref-type val))

    (-> vm
        inc-ip
        (assoc :stack (conj stack val)))))

(defn vm-plus
  [{:keys [stack frames] :as vm}]

  (when-not (>= (count stack) 2)
    (throw (Exception. (format "plus requires two operands on the stack, found %s" (count stack)))))

  (let [len (count stack)
        b (nth stack (- len 1))
        a (nth stack (- len 2))
        result (+ a b)
        stack (-> stack
                  pop
                  pop
                  (conj result))]

    (log vm (format "plus %s %s = %s" a b result))

    (-> vm
        inc-ip
        (assoc :stack stack))))

(defn vm-pop
  [{:keys [stack frames] :as vm} [ref-type ref-arg :as args]]
  (let [head (current-stack-value vm)]

    (case ref-type

      ;; pop to a local
      :local (let [{:keys [locals] :as frame} (current-frame vm)
                   local-index ref-arg]

               (when-not (integer? local-index)
                 (throw (Exception. (format "local-index must be an integer" args))))

               (log vm (format "pop %s to local[%s]" head local-index))

               (-> vm
                   inc-ip
                   (update-in [:frames (.indexOf frames frame) :locals] #(assoc % local-index head))
                   (update-in [:stack] pop)))

      ;; just pop the value
      nil (do
            (log vm "pop")
            (-> vm
                inc-ip
                (assoc :stack (pop stack)))))))

(defn vm-call
  [{:keys [stack frames ip aliases] :as vm} [ref-type ref-val :as ref]]
  (log vm (format "call %s %s" ref-type ref-val))

  (let [new-ip (get-addr vm ref)]
    (assoc vm
           :ip new-ip
           :frames (conj frames {:saved-stack stack
                                 :saved-ip (inc ip)
                                 :locals {}})
           :stack [])))

(defn vm-ret
  [{:keys [stack frames] :as vm}]
  (do
    (log vm (format "ret"))

    (let [{:keys [saved-stack saved-ip] :as frame} (current-frame vm)]
       (assoc vm
              :ip saved-ip
              :frames (pop frames) 
              :stack (if (seq stack)                       ;; if the stack is not empty, push the top item
                       (conj saved-stack (peek stack))     ;; into the parent stack as a 'return' value
                       saved-stack)))))

(defn vm-jump
  [{:keys [stack frames] :as vm} jump-ref]

  (let [head (current-stack-value vm)
        new-ip (get-addr vm jump-ref)]
    (log vm (format "jump-ing to %s" new-ip))
    (assoc vm :ip new-ip)))

(defn vm-jump-if
  [{:keys [stack frames] :as vm} jump-ref]

  (let [head (current-stack-value vm)
        match (and (not (nil? head)) (not (zero? head)))
        new-ip (get-addr vm jump-ref)]
    (if match
      (do
        (log vm (format "jump-if matched, jumping to %s" new-ip))
        (assoc vm :ip new-ip))
      (do
        (log vm (format "jump-if not matched, not jumping to %s" new-ip))
        (inc-ip vm)))))

;; the leftmost 3 bits are used to encode the following types:
;; -------------
;; :unsigned-int
;; :bool
;; :nil
;; :string
;; :record

(def max-unsigned-int (bit-not (bit-shift-left 7 61)))

(defn pack-val
  [{:keys [type data]}]

  (let [[t v] (case type
                :unsigned-int (let [cmp (Long/compareUnsigned data max-unsigned-int)]
                                (when (> cmp 0)
                                  (throw (Exception. (format "cannot represent unsigned integers greater than %s: %s"
                                                             max-unsigned-int data))))
                                [0 data])
                :bool         [1 (if data 1 0)]
                :nil          [2 0]
                :object (let [cmp (Long/compareUnsigned data max-unsigned-int)]
                                (when (> cmp 0)
                                  (throw (Exception. (format "cannot represent objects with a location greater than %s: %s"
                                                             max-unsigned-int data))))
                                [3 data]))]
        (bit-or (bit-shift-left t 61) v)))

(defn unpack-val
  [packed]
  (let [t (bit-shift-right packed 61) 
        v (bit-and max-unsigned-int packed)
        [type data] (case t
                      0 [:unsigned-int v]
                      1 [:bool (if (zero? v) false true)]
                      2 [:nil nil]
                      3 [:object v])]
    {:type type
     :data data}))

(defn value-is-object?
  [v]
  (= (-> v unpack-val :type) :object))

(defn vm-gc-clear
  [{:keys [heap] :as vm}]
  (assoc vm :heap (->> (for [obj heap]
                         (assoc :seen false))
                       (into []))))

(defn vm-gc-collect-roots
  [{:keys [stack frames] :as vm}]
  (->> frames
       (map (fn [frame]
              (concat (:saved-stack frame) (:locals frame))))
       (concat stack)
       (filter value-is-object?)
       (into [])))

(defn vm-gc-mark-root
  [heap {:keys [type data seen] :as root}]

  (if-not seen
    (loop [i 0, heap heap]
      (if-not (= i (count data))

        (let [child-idx (-> (aget data i) unpack-val :data)
              child-obj (get heap child-idx)
              heap (if (= (:type child-obj) :object)
                     (vm-gc-mark-root heap child-obj)
                     heap)]
          (recur (inc i) heap))

        (assoc-in heap [data :seen] true)))
    heap))

(defn vm-gc-mark
  [{:keys [stack frames heap] :as vm}]

  (let [marked-heap (reduce
                     (fn [heap val]
                       (vm-gc-mark-root heap (unpack-val val)))
                     heap
                     (vm-gc-collect-roots vm))]
    (assoc vm :heap heap)))

(defn vm-gc-sweep
  [{:keys [stack frames heap] :as vm}]

  (let [[heap old->new]
        (->> heap
             (map-indexed (fn [idx obj]
                            (when (:seen obj)
                              (let [val (pack-val {:type :object
                                                   :data idx})]
                                [val obj]))))
             (filter identity)
             (reduce
              (fn [[new-heap old->new] [old-val obj]]
                (let [new-val (pack-val {:type :object
                                         :data (count new-heap)})]
                  [(conj new-heap obj) (assoc old->new old-val new-val)]))
              [[] {}]
              heap))

        migrate-fn (fn [old-val]
                     (let [{:keys [type data] :as old} (unpack-val old-val)]
                       (if-not (= type :object)
                         old-val
                         (let [new-val (get old->new old-val)]
                           (when-not new-val
                             (throw (Exception. (format "no new for old: %s" old))))))))

        migrate-vec-fn (fn [v]
                         (->> v
                              (map migrate-fn)
                              (into [])))

        stack (migrate-vec-fn stack)

        frames (->> frames
                    (map #(-> %
                              (update-in [:saved-stack] migrate-vec-fn)
                              (update-in [:locals] migrate-vec-fn)))
                    (into []))]

    (assoc vm
           :heap heap
           :stack stack
           :frames frames)))

;; mark all objects 'unseen'
;; traverse roots, marking objects 'seen' and noting which locations reference them
;; copy all seen objects to new vector
;; update all references to new id's

(defn vm-gc
  [{:keys [stack frames heap] :as vm}]
  (-> vm
      vm-gc-clear
      vm-gc-mark
      vm-gc-sweep))

(defn vm-alloc
  [{:keys [stack heap] :as vm} [object-type length]]

  (let [data (long-array length)
        _ (let [null (pack-val {:type :nil})]
            (doseq [i (range length)]
              (aset data i null)))
        obj {:type object-type
             :data data}
        heap (conj heap obj)
        stack (conj stack (.indexOf heap obj))]

    (log vm (format "alloc %s(%s)" object-type length))

    (-> vm
        inc-ip
        (assoc :stack stack
               :heap heap))))

(defn run-vm
  [{:keys [instructions ip] :as vm}]

  (let [[inst args] (nth instructions ip)
        vm (case inst
             :halt    (do
                        (log vm "halting")
                        #_(pprint vm))
             :push    (vm-push vm args)
             :plus    (vm-plus vm)
             :pop     (vm-pop  vm args)
             :call    (vm-call vm args)
             :ret     (vm-ret  vm)
             :jump    (vm-jump vm args)
             :jump-if (vm-jump-if vm args)
             :alloc   (vm-alloc vm args)
             (log vm (format "invalid instruction %s %s, halting" inst args)))]
    (when vm
      (recur vm))))

(comment

  (-> [;; procedure 'f'
       'f
       [:alloc [:string 100]]
       [:push [:const 99]]
       [:pop  [:local 0]]
       [:push [:local 0]]
       [:push [:args 0]]
       [:push [:const 0]]
       [:jump-if [:alias 'skip]]
       [:push [:args 1]]
       [:plus]
       'skip
       [:plus]
       [:push [:const 50]]
       [:plus]
       [:ret]
       ;; entry point
       'main
       [:push [:const 100]]
       [:push [:const 200]]
       [:call [:alias 'f]]
       [:pop]
       [:halt]
       ]
      make-vm
      run-vm)


  )
