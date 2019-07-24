(ns jib.il)

(defn fail [& msgs]
  (throw (Exception. (apply str "il failed: " msgs))))

(defn classify-emit [m]
  (cond
    (::reg-addr m) ::reg-addr
    (::literal m) ::literal
    (::sym m) ::sym
    (string? m) ::str
    (int? m) ::int
    (::pushq m) ::pushq
    (::popq m) ::popq
    (::addq m) ::addq
    (::imulq m) ::imulq
    (::idivq m) ::idivq
    (::cdq m) ::cdq
    (::subq m) ::subq
    (::call m) ::call
    (::call-addr m) ::call-addr
    (::cmpq m) ::cmpq
    (::je m) ::je
    (::jl m) ::jl
    (::jg m) ::jg
    (::jge m) ::jge
    (::jle m) ::jle
    (::jne m) ::jne
    (::jmp m) ::jmp
    (::movq m) ::movq
    (::leaq m) ::leaq
    (::leave m) ::leave
    (::ret m) ::ret
    (::label m) ::label
    (::label-addr m) ::label-addr
    (::globl m) ::globl
    (::quad m) ::quad
    (::quad-array m) ::quad-array
    (::cstring-data m) ::cstring-data
    (::local m) ::local
    (::line-comment m) ::line-comment
    :else   [m]))

(defmulti emit* #'classify-emit)

(defn instruction
  ([id]
   (instruction id []))
  ([id args]
  (str "\t" (name id)
       (when (seq args)
         (str "\t" (->> args
                        (map emit*)
                        (clojure.string/join ", ")))))))

(defmethod emit* ::reg-addr
  [{:keys [::reg-addr ::offset]}]
  (if-not offset
    (str "%" (name reg-addr))
    (str offset "(%" (name reg-addr) ")" )))

(defmethod emit* ::label-addr
  [{:keys [::label-addr ::reg]}]
    (str (name label-addr) "(%" (name reg) ")"))

(defmethod emit* ::literal
  [{:keys [::literal]}]
  (str "$" literal))

(defmethod emit* ::sym
  [{:keys [::sym]}]
  (name sym))

(defmethod emit* ::pushq
  [{:keys [::pushq]}]
  (instruction :pushq [pushq]))

(defmethod emit* ::popq
  [{:keys [::popq]}]
  (instruction :popq [popq]))

(defmethod emit* ::addq
  [{:keys [::src ::dest]}]
  (instruction :addq [src dest]))

(defmethod emit* ::imulq
  [{:keys [::src ::dest]}]
  (instruction :imulq [src dest]))

(defmethod emit* ::idivq
  [{:keys [::src]}]
  (instruction :idivq [src]))

(defmethod emit* ::cdq
  [{:keys []}]
  (instruction :cdq []))

(defmethod emit* ::subq
  [{:keys [::src ::dest]}]
  (instruction :subq [src dest]))

;; call is a sym
(defmethod emit* ::call
  [{:keys [::call]}]
  (instruction :call [call]))

(defmethod emit* ::call-addr
  [{:keys [::call-addr]}]
  (instruction :call [call-addr]))

(defmethod emit* ::cmpq
  [{:keys [::first ::second]}]
  (instruction :cmpq [first second]))

(defmethod emit* ::je
  [{:keys [::je]}]
  (instruction :je [je]))

(defmethod emit* ::jl
  [{:keys [::jl]}]
  (instruction :jl [jl]))

(defmethod emit* ::jg
  [{:keys [::jg]}]
  (instruction :jg [jg]))

(defmethod emit* ::jge
  [{:keys [::jge]}]
  (instruction :jge [jge]))

(defmethod emit* ::jle
  [{:keys [::jle]}]
  (instruction :jle [jle]))

(defmethod emit* ::jne
  [{:keys [::jne]}]
  (instruction :jne [jne]))

(defmethod emit* ::jmp
  [{:keys [::jmp]}]
  (instruction :jmp [jmp]))

(defmethod emit* ::movq
  [{:keys [::src ::dest]}]
  (instruction :movq [src dest]))

(defmethod emit* ::leaq
  [{:keys [::src ::dest]}]
  (instruction :leaq [src dest]))

(defmethod emit* ::leave
  [{:keys []}]
  (instruction :leave []))

(defmethod emit* ::ret
  [{:keys []}]
  (instruction :ret []))

(defmethod emit* ::label
  [{:keys [::label]}]
  (str label ":"))

(defmethod emit* ::globl
  [{:keys [::globl]}]
  (instruction :.globl [globl]))

(defmethod emit* ::quad
  [{:keys [::quad]}]
  (instruction :.quad [quad]))

(defmethod emit* ::quad-array
  [{:keys [::quad-array]}]
  (instruction :.quad quad-array))

(defmethod emit* ::cstring-data
  [{:keys [::cstring-data]}]
  (instruction :.asciz [(str "\"" cstring-data "\"")]))

(defmethod emit* ::local
  [{:keys [::local]}]
  (str local ":"))

(defmethod emit* ::str
  [s] s)

(defmethod emit* ::int
  [i] (str i))

(defmethod emit* ::line-comment
  [{:keys [::line-comment]}]
  (str "# " line-comment))

(defn emit [n]
  (->> n
       flatten
       (filter identity)
       (map emit*)
       (clojure.string/join "\n")))

(comment 
  (reg-addr :rbp)

  (println
   (emit* (popq (reg-addr :rbp))))

  (println
   (emit* (addq (literal 100) (reg-addr :rax))))

  (println
   (emit* (call (sym :hello-world))))

  (label)

  (println (emit* (globl (sym :hi))))

  (println
   (emit
    (section :rodata [(globl (sym :hi))
                      (globl (sym :there))])))

  )

;; memory addressing
;; r is keyword, offset is signed integer
(defn reg-addr
  ([r] (reg-addr r nil))
  ([r offset]
   (-> {::reg-addr r}
       (#(if offset (assoc % ::offset offset) %)))))

;; always relative to a pointer
(defn label-addr
  ([name reg]
   (when-not (or (symbol? name) (keyword? name))
     (throw (Exception. (str "not a symbol: " name))))
   {::label-addr name ::reg reg}))

(defn literal
  [l] {::literal l})

(defn sym
  [s] {::sym s})

;; instructions
(defn pushq
  [addr] {::pushq addr})

(defn popq
  [addr] {::popq addr})

(defn addq
  [src dest] {::addq true ::src src ::dest dest})

(defn imulq
  [src dest] {::imulq true ::src src ::dest dest})

(defn idivq
  [src] {::idivq true ::src src})

(defn cdq
  [] {::cdq true})

(defn subq 
  [src dest] {::subq true ::src src ::dest dest})

(defn call
  [s] {::call (name s)})

(defn call-addr
  [s] {::call-addr s})

(defn cmpq
  [a b] {::cmpq true ::first a ::second b})

;; this ony supports labels, not raw jumps right now
(defn je
  [label] {::je (::label label)})

(defn jl
  [label] {::jl (::label label)})

(defn jg
  [label] {::jg (::label label)})

;; this ony supports labels, not raw jumps right now
(defn jge
  [label] {::jge (::label label)})

;; this ony supports labels, not raw jumps right now
(defn jle
  [label] {::jle (::label label)})

;; this ony supports labels, not raw jumps right now
(defn jne
  [label] {::jne (::label label)})

;; this ony supports labels, not raw jumps right now
(defn jmp
  [label]
  {::jmp (::label label)})

(defn movq
  [src dest]
  (when (nil? src)
    (fail "src is required: " src))
  (when (nil? dest)
    (fail "dest is required: " dest))
  {::movq true ::src src ::dest dest})

(defn leaq
  [src dest] {::leaq true ::src src ::dest dest})

(defn leave
  [] {::leave true})

(defn ret
  [] {::ret true})

(defn label
  ([] (label (gensym "label")))
  ([i] {::label (name i)}))

;; directives
(defn globl
  [s] {::globl s})

(defn quad
  [q] {::quad q})

(defn quad-array
  [q] {::quad-array q})

(defn cstring-data
  [s] {::cstring-data s})

;; meta stuff
;;
;; the idea here is that we need to accumulate local variables in the stack over time
;; when compiling, we should 
(defn local
  [] {::local (gensym "local")})

(defn line-comment
  [line-comment] {::line-comment line-comment})

;; sections

;; valid section types are:
;;   :text
;;   :rodata
;;   :rwdata
(defn section [section-type emittable]
  (let [section (case section-type
                  :text (instruction :.text)
                  :rwdata (instruction :.section [".rwdata" "\"w\""]))]
    [section emittable]))



