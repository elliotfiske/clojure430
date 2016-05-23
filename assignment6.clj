; Declare functions to make compiler happy
(declare parse-if)
(declare binop?)
(declare parse-binop)
(declare parse-app)
(declare parse-lam)
(declare parse)
(declare desugar-arg)
(declare desugar-param)
(declare desugar-with)
(declare check-equal?)

; Test Function
(def check-equal? 
    (fn [_test result]
        (if (= _test result)
            (println "pass")
            (println "fail")
        )   
    )
)

; ExprC defs
(defrecord NumC [n])
(defrecord BoolC [b])
(defrecord IdC [i])
(defrecord IfC [c t e])
(defrecord BinopC [o l r])
(defrecord AppC [f a])
(defrecord LamC [p b])

; Parse IfC's
(def parse-if
    (fn [sexp]
        (let [
            c (parse (first (rest sexp)))   
            t (parse (first (rest (rest sexp))))   
            e (parse (first (rest (rest (rest sexp)))))   
        ]
            (IfC. c t e)
        )
    )
)

; Check if symbol is a binop
(def binop?
    (fn [o]
        (if (or 
            (= o '+)  
            (= o '-)  
            (= o '*)  
            (= o '/)  
            (= o '<=)  
            (= o 'eq?)  
        )
            true
            false
        ) 
    )
)

; Parse BinopC's
(def parse-binop 
    (fn [sexp]
        (let [
            o (first sexp)
            l (parse (first (rest sexp)))   
            r (parse (first (rest (rest sexp))))   
        ]
            (BinopC. o l r)
        )
    )
)

; Parse AppC's
(def parse-app
    (fn [sexp]
        (let [
           f (parse (first sexp))
           a (map parse (rest sexp))
        ]
           (AppC. f a)  
        )          
    )
)

; Parse LamC's
(def parse-lam
    (fn [sexp]
        (let [
            p (first (rest sexp))
            b (parse (last sexp))
        ]
            (LamC. p b)
        )          
    )
)

; Desugar the arg for with
(def desugar-arg
   (fn [sexp]
      (parse (last sexp))
   )
)

; Desugar the param for with
(def desugar-param
   (fn [sexp]
      (first sexp)
   )
)

; Desugar with
(def desugar-with
    (fn [sexp]
        (let [
            v (rest (butlast sexp))
            b (parse (last sexp))
        ]
            (AppC. (LamC. (map desugar-param v) b) (map desugar-arg v))
        )
    )
)

; Main parsing fuction
; List -> ExprC's
(def parse 
    (fn [sexp] 
        (if (number? sexp)
            (NumC. sexp)
            (if (or (= sexp 'true) (= sexp 'false))
                (BoolC. sexp)
                (if (symbol? sexp)
                    (IdC. sexp)
                    (if (= (first sexp) 'if)
                        (parse-if sexp)
                        (if (= (first sexp) 'with)
                            (desugar-with sexp)
                            (if (= (first sexp) 'lam) 
                                (parse-lam sexp)
                                (if (binop? (first sexp))
                                    (parse-binop sexp)
                                    (parse-app sexp)    
                                )
                            )
                        )
                    )
                )
            )
        )
    )
)

(check-equal? (parse '5) (NumC. 5))
(check-equal? (parse 'true) (BoolC. true))
(check-equal? (parse 'false) (BoolC. false))
(check-equal? (parse 'x) (IdC. 'x))
(check-equal? (parse '(if true 1 0)) (IfC. (BoolC. true) (NumC. 1) (NumC. 0)))
(check-equal? (parse '(+ 1 2)) (BinopC. '+ (NumC. 1) (NumC. 2)))
(check-equal? (parse '(lam (a b c) 3)) (LamC. (list 'a 'b 'c) (NumC. 3)))
(check-equal? (parse '((lam (a b c) 3) 1 2 3)) (AppC. (LamC. (list 'a 'b 'c) (NumC. 3)) (list (NumC. 1) (NumC. 2) (NumC. 3))))
(check-equal? (parse '(with (z = (+ 9 14)) (y = 98) (+ z y))) 
   (AppC. (LamC. (list 'z 'y) (BinopC. '+ (IdC. 'z) (IdC. 'y))) (list (BinopC. '+ (NumC. 9) (NumC. 14)) (NumC. 98))))


