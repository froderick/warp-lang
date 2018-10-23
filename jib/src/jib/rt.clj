(ns jib.rt
  (:require [jib.il :as il]))

;; in X86_64 all pointers are 8 bytes
(def pointer-length 8)

;; invoking runtime functionality

(def global-main-rbp :main_rbp)
(def global-rt-instance :rt_instance)

(def heap-bytes (* 1024 1000))

(defn entry-point-data
  []
  [(il/line-comment "entry-point-data")
   (il/section :rwdata [(il/label global-main-rbp)
                        (il/quad 0)
                        (il/label global-rt-instance)
                        (il/quad 0)])])

(def main-rbp-addr (il/label-addr global-main-rbp :rip))
(def rt-instance-addr (il/label-addr global-rt-instance :rip))

(defn entry-point-begin
  [func-name]
  [(il/line-comment "init runtime")

   ; make function visible to linker
   (il/globl func-name)

   ; store main $rbp in global var
   (il/movq (il/reg-addr :rbp) main-rbp-addr)

   ; create rt instance, store in global var
   (il/movq main-rbp-addr (il/reg-addr :rdi))
   (il/movq (il/literal heap-bytes) (il/reg-addr :rsi))
   (il/call :_rt_create)
   (il/movq (il/reg-addr :rax) rt-instance-addr)])

(defn entry-point-end
  []
  [(il/line-comment "destroy runtime")

   ; saving %rax on the stack
   (il/pushq (il/reg-addr :rax))

   (il/movq rt-instance-addr (il/reg-addr :rdi))

   ; this extra 8 bytes of padding is here to avoid the stack_not_16_byte_aligned_error in osx
   ; TODO: perhaps this can be computed dynamically?
   (il/subq (il/literal pointer-length) (il/reg-addr :rsp)) 
   (il/call :_rt_destroy)
   (il/addq (il/literal pointer-length) (il/reg-addr :rsp))

   ; putting rax back
   (il/popq (il/reg-addr :rax))])

(defn truthy
  [addr]
  [(il/movq addr (il/reg-addr :rdi))
   (il/call :_rt_val_truthy)])

(defn- alloc-params
  "Sets up the first three parameters rt allocation functions always take."
  []
  [(il/movq rt-instance-addr (il/reg-addr :rdi))
   (il/movq (il/reg-addr :rbp) (il/reg-addr :rsi))
   (il/movq (il/reg-addr :rsp) (il/reg-addr :rdx))])

(defn alloc-nil
  []
  [(alloc-params)
   (il/call :_rt_alloc_nil)])

(defn alloc-list
  []
  [(alloc-params)
   (il/call :_rt_alloc_list)])

(defn alloc-int
  "allocates a literal integer from the runtime"
  [i]
  [(alloc-params)
   (il/movq (il/literal i) (il/reg-addr :rcx))
   (il/call :_rt_alloc_int)])

(defn alloc-int-addr
  "allocates an integer based on the 8 bytes found at a particular memory location"
  [loc]
  [(alloc-params)
   (il/movq loc (il/reg-addr :rcx))
   (il/call :_rt_alloc_int)])

(defn alloc-bool
  "allocates a literal bool from the runtime"
  [b]
  [(alloc-params)
   (il/movq (il/literal (if b 1 0)) (il/reg-addr :rcx))
   (il/call :_rt_alloc_bool)])

(defn alloc-bool-addr
  "allocates a bool based on the 8 bytes found at a particular memory location"
  [loc]
  [(alloc-params)
   (il/movq loc (il/reg-addr :rcx))
   (il/call :_rt_alloc_bool)])

(defn alloc-str-addr
  "allocates a string based on the null-terminated c string found at a particular memory location"
  [loc]
  [(alloc-params)
   (il/leaq loc (il/reg-addr :rcx))
   (il/call :_rt_alloc_str)])

(defn alloc-fn
  "allocates a first-class function from the runtime"
  [fn-ptr-addr]
  [(alloc-params)
   (il/movq fn-ptr-addr (il/reg-addr :rcx))
   (il/call :_rt_alloc_fn)])

(defn val-int
  "returns the int value wrapped by the runtime object pointer"
  [int-addr]
  [(il/movq int-addr (il/reg-addr :rdi))
   (il/call :_rt_val_int)])

(defn println-addr
  "prints the value for the supplied pointer
   returns nil"
  [str-addr]
  [(il/movq str-addr (il/reg-addr :rdi))
   (il/call :_rt_println)
   (alloc-nil)])

(defn val-type
  "returns an int describing a value's type"
  [addr]
  [(il/movq addr (il/reg-addr :rdi))
   (il/call :_rt_val_type)])

(defn val-equals-addr
  "returns a bool indicating whether two values are equal"
  [a-addr b-addr]
  [(il/movq a-addr (il/reg-addr :rdi))
   (il/movq b-addr (il/reg-addr :rsi))
   (il/call :_rt_val_equals)])

(defn val-count
  "returns the number of items in a list"
  [list-addr]
  [(alloc-params)
   (il/movq list-addr (il/reg-addr :rcx))
   (il/call :_rt_val_count)])

(defn val-cons
  "returns the number of items in a list"
  [x-addr list-addr]
  [(alloc-params)
   (il/movq x-addr (il/reg-addr :rcx))
   (il/movq list-addr (il/reg-addr :r8))
   (il/call :_rt_val_cons)])

(defn val-first
  "returns the number of items in a list"
  [list-addr]
  [(il/movq list-addr (il/reg-addr :rdi))
   (il/call :_rt_val_first)])

(defn val-fn-ptr
  "returns the address of the start of the function"
  [fn-addr]
  [(il/movq fn-addr (il/reg-addr :rdi))
   (il/call :_rt_val_fn_ptr)])

(defn val-rest
  "returns the number of items in a list"
  [list-addr]
  [(alloc-params)
   (il/movq list-addr (il/reg-addr :rcx))
   (il/call :_rt_val_rest)])
