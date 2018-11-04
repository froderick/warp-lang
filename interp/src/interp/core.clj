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

  (let [b (first stack)
        a (second stack)
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
  [{:keys [stack frames ip aliases] :as vm} [ref-type ref-val]]
  (log vm (format "call %s %s" ref-type ref-val))

  (let [new-ip (case ref-type
                 :address ref-val
                 :alias (let [addr (get aliases ref-val)]
                          (when-not addr
                            (throw (Exception. (format "no such alias found: %s" ref-val))))
                          addr))]
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

(defn run-vm
  [{:keys [instructions ip] :as vm}]

  (let [[inst args] (nth instructions ip)
        vm (case inst
             :halt    (log vm "halting")
             :push    (vm-push vm args)
             :plus    (vm-plus vm)
             :pop     (vm-pop  vm args)
             :call    (vm-call vm args)
             :ret     (vm-ret  vm)
             :jump    (vm-jump vm args)
             :jump-if (vm-jump-if vm args)
             (log vm (format "invalid instruction %s %s, halting" inst args)))]
    (when vm
      (recur vm))))

(comment

  (-> [;; procedure 'f'
       'f
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
