
(defrecord NumC [n])
(defrecord IdC [x])

(defrecord NumV [n])

(def top-env {:+ :+Prim
              :- :-Prim
              :* :*Prim
              :/ :/Prim
              :<= :<=Prim
              :eq? :eq?Prim})

; All the primitive functions live here!

; Primitive plus
; (: my+ ((list NumV NumV) -> NumV)
(def my+ (fn [args]
           ))

(def primitive-fundefs
  {:+Prim my+
   :-Prim my-})

; Interp takes in an ExprC and evaluates it.
; (: interp (ExprC -> Value))
(def interp (fn [a env]
   (if (instance? NumC a)
      (NumV. (:n a))
      (if (instance? IdC a)
        (if )
   ))))

(println (interp (NumC. 3))) ; (NumV 3)
