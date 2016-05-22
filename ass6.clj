
(defrecord NumC [n])
(defrecord IdC [x])
(defrecord IfC [condition then else])
(defrecord BoolC [b])

(defrecord NumV [n])
(defrecord BoolV [b])

(declare interp-fn)
(declare interp)

(def top-env {:+ :+Prim
              :- :-Prim
              :* :*Prim
              :div :divPrim
              :<= :<=Prim
              :eq? :eq?Prim})

; All the primitive functions live here!

; Primitive plus
; (: my+ ((list NumV NumV) -> NumV)
(def my+ (fn [args]
           ))

(def my- (fn [args]
          ))

(def primitive-fundefs
  {:+Prim my+
   :-Prim my-})


; Interp takes in an ExprC and evaluates it.
; (: interp (ExprC map -> Value))
(def interp (fn [a env]
   ; BoolC
   (if (instance? BoolC a)
     ;(BoolV. (:b a))
     (BoolV. true)

     ; NumC
     (if (instance? NumC a)
        (NumV. (:n a))

        ; IdC
        (if (instance? IdC a)
          (if (contains? env (:x a))
            (get env (:x a))
            ((print "DFLY: No variable called ") (print (:x a))))

          ; IfC
          (if (instance? IfC a)
            (((def condition (interp (:condition a) env)))
             (print condition)
            (if (instance? BoolV condition)
              (if (:b condition)
                   (interp (:then a) env)
                   (interp (:else a) env))
              ((print "DFLY: An if statement condition
                     evaluated to something other than true or false: ")
              (print condition))))

              ; LamC and AppC
              (interp-fn a env)
            )

   )))))


; Helper function that interps LamCs and AppCs
; (: interp-fn (ExprC map -> Value))
; (def interp-fn )


(println (interp (NumC. 3) top-env)) ; (NumV 3)
(println (interp (IdC. :+) top-env)) ; :+Prim
(println (interp (BoolC. true) top-env)) ; (BoolV true)
(println (interp (IfC. (BoolC. false) (NumC. 4) (NumC. 5)) top-env)) ; (NumV 5)
