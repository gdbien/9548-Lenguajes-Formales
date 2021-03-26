(require '[clojure.test :refer [is deftest run-tests]])

(load-file "basic.clj")

(deftest test-palabra-reservada?
   (is (= true (palabra-reservada? 'REM)))
   (is (= false (palabra-reservada? 'SPACE)))
)

(deftest test-operador?
   (is (= true (operador? '+)))
   (is (= true (operador? (symbol "+"))))
   (is (= false (operador? (symbol "%"))))
)

(deftest test-anular-invalidos
   (is (= '(IF X nil * Y < 12 THEN LET nil X = 0) (anular-invalidos '(IF X & * Y < 12 THEN LET ! X = 0))))   
)

(deftest test-cargar-linea
   (is (= '[((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '(10 (PRINT X)) [() [:ejecucion-inmediata 0] [] [] [] 0 {}])))
   (is (= '[((10 (PRINT X)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '(20 (X = 100)) ['((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))
   (is (= '[((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '(15 (X = X + 1)) ['((10 (PRINT X)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))
   (is (= '[((10 (PRINT X)) (15 (X = X - 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '(15 (X = X - 1)) ['((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))
   ;Borrado
   (is (= '[() [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '(10) ['((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))
   ;Mas de una sentencia por linea
   (is (= '[((10 (PRINT X) (PRINT Y))) [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '(10 (PRINT X) (PRINT Y)) [() [:ejecucion-inmediata 0] [] [] [] 0 {}])))
   ;Linea vacía
   (is (= '[() [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '() ['() [:ejecucion-inmediata 0] [] [] [] 0 {}])))
)

(deftest test-expandir-nexts
   (is (= '((PRINT 1) (NEXT A) (NEXT B)) (expandir-nexts '((PRINT 1) (NEXT A , B)))))
   (is (= '((PRINT 1) (NEXT A) (NEXT B) (NEXT C)) (expandir-nexts '((PRINT 1) (NEXT A , B , C)))))
   (is (= '((NEXT A) (NEXT B) (PRINT 1) (NEXT C) (NEXT D)) (expandir-nexts '((NEXT A , B) (PRINT 1) (NEXT C , D)))))
   (is (= '((NEXT I) (NEXT J)) (expandir-nexts '((NEXT I , J)))))
   (is (= '((NEXT I) (NEXT J)) (expandir-nexts (list (list 'NEXT 'I (symbol ",") 'J)))))
)

(deftest test-dar-error
   (is (= "?SYNTAX ERROR" (with-out-str (dar-error 16 [:ejecucion-inmediata 4]))))
   (is (= nil (dar-error 16 [:ejecucion-inmediata 4])))
   (is (= "?ERROR DISK FULL" (with-out-str (dar-error "?ERROR DISK FULL" [:ejecucion-inmediata 4]))))
   (is (= nil (dar-error "?ERROR DISK FULL" [:ejecucion-inmediata 4])))
   (is (= "?SYNTAX ERROR IN 100" (with-out-str (dar-error 16 [100 3]))))
   (is (= nil (dar-error 16 [100 3])))
   (is (= "?ERROR DISK FULL IN 100" (with-out-str (dar-error "?ERROR DISK FULL" [100 3]))))
   (is (= nil (dar-error "?ERROR DISK FULL" [100 3])))
)

(deftest test-variable-float?
   (is (= true (variable-float? 'X)))
   (is (= false (variable-float? 'X%)))
   (is (= false (variable-float? 'X$)))  
)

(deftest test-variable-integer?
   (is (= true (variable-integer? 'X%)))
   (is (= false (variable-integer? 'X)))
   (is (= false (variable-integer? 'X$)))   
)

(deftest test-variable-string?
   (is (= true (variable-string? 'X$)))
   (is (= false (variable-string? 'X)))
   (is (= false (variable-string? 'X%)))   
)

(deftest test-contar-sentencias
   (is (= 2 (contar-sentencias 10 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))   
   (is (= 1 (contar-sentencias 15 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
   (is (= (contar-sentencias 20 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
)

(deftest test-buscar-lineas-restantes
   (is (= nil (buscar-lineas-restantes [() [:ejecucion-inmediata 0] [] [] [] 0 {}])))
   (is (= nil (buscar-lineas-restantes ['((PRINT X) (PRINT Y)) [:ejecucion-inmediata 2] [] [] [] 0 {}])))
   (is (= (list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 2] [] [] [] 0 {}])))   
   (is (= (list '(10 (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
   (is (= (list '(10) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 0] [] [] [] 0 {}])))
   (is (= (list '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [] [] [] 0 {}])))
   (is (= (list '(15) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 0] [] [] [] 0 {}])))
   (is (= '((20 (NEXT I) (NEXT J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}])))   
   (is (= '((20 (NEXT I) (NEXT J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 2] [] [] [] 0 {}])))
   (is (= '((20 (NEXT J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 1] [] [] [] 0 {}])))
   (is (= '((20)) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 0] [] [] [] 0 {}])))
   (is (= '((20)) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 -1] [] [] [] 0 {}])))
   (is (= nil (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [25 0] [] [] [] 0 {}])))
)

(deftest test-continuar-linea
   (is (= [nil [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}]] (continuar-linea [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}])))
   (is (= "?RETURN WITHOUT GOSUB ERROR IN 20" (with-out-str (continuar-linea [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}]))))
   (is (= [':omitir-restante [(list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [] [] [] 0 {}]] (continuar-linea [(list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [[15 2]] [] [] 0 {}])))
)

(deftest test-extraer-data
   (is (= '() (extraer-data '(()))))
   (is (= '("HOLA" "MUNDO" 10 20) (extraer-data (list '(10 (PRINT X) (REM ESTE NO) (DATA 30)) '(20 (DATA HOLA)) (list 100 (list 'DATA 'MUNDO (symbol ",") 10 (symbol ",") 20))))))
)

(deftest test-ejecutar-asignacion
   (is (= '[((10 (PRINT X))) [10 1] [] [] [] 0 {X 5}] (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 {}])))
   (is (= '[((10 (PRINT X))) [10 1] [] [] [] 0 {X 5}] (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])))
   (is (= '[((10 (PRINT X))) [10 1] [] [] [] 0 {X 3}] (ejecutar-asignacion '(X = X + 1) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])))
   (is (= '[((10 (PRINT X))) [10 1] [] [] [] 0 {X$ "HOLA MUNDO"}] (ejecutar-asignacion '(X$ = X$ + " MUNDO") ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}])))
   (is (= '[((10 (PRINT X))) [10 1] [] [] [] 0 {X 10 L 10}] (ejecutar-asignacion '(X = L) '[((10 (PRINT X))) [10 1] [] [] [] 0 {X 0 L 10}])))
   (is (= '[((10 (PRINT X))) [10 1] [] [] [] 0 {S$ ""}] (ejecutar-asignacion '(S$ = "") '[((10 (PRINT X))) [10 1] [] [] [] 0 {}])))
   (is (= '[((10 (PRINT X))) [10 1] [] [] [] 0 {S$ "*"}] (ejecutar-asignacion '(S$ = S$ + "*") '[((10 (PRINT X))) [10 1] [] [] [] 0 {S$ ""}])))
   (is (= '[((10 (PRINT X))) [10 1] [] [] [] 0 {S$ "Y"}] (ejecutar-asignacion '(S$ = "Y") '[((10 (PRINT X))) [10 1] [] [] [] 0 {}])))
)


(deftest test-preprocesar-expresion
   (is (= '("HOLA" + " MUNDO" + "") (preprocesar-expresion '(X$ + " MUNDO" + Z$) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}])))
   (is (= '(5 + 0 / 2 * 0) (preprocesar-expresion '(X + . / Y% * Z) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 5 Y% 2}]))) 
   (is (= '(2 + 5) (preprocesar-expresion '(X + 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}]))) 
   (is (= '(2 <= 4) (preprocesar-expresion '(X <= 4) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])))
   (is (= '("ESTRELLA: " + "*") (preprocesar-expresion '(S$ + "*") ['((10 (PRINT X))) [10 1] [] [] [] 0 '{S$ "ESTRELLA: "}])))
   (is (= '("Y") (preprocesar-expresion '("Y") ['((10 (PRINT X))) [10 1] [] [] [] 0 '{}])))
)

(deftest test-desambiguar
   (is (= (list '-u 2 '* (symbol "(") '-u 3 '+ 5 '- (symbol "(") 2 '/ 7 (symbol ")") (symbol ")")) (desambiguar (list '- 2 '* (symbol "(") '- 3 '+ 5 '- (symbol "(") '+ 2 '/ 7 (symbol ")") (symbol ")")))))   
   (is (= (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ")")) (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ")")))))
   (is (= (list 'MID3$ (symbol "(") 1 (symbol ",") 2 (symbol ",") 3 (symbol ")")) (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ",") 3 (symbol ")")))))
   (is (= (list 'MID3$ (symbol "(") 1 (symbol ",") '-u 2 '+ 'K (symbol ",") 3 (symbol ")")) (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") '- 2 '+ 'K (symbol ",") 3 (symbol ")")))))
   (is (= '(2 + 5) (desambiguar '(2 + 5))))
)

(deftest test-precedencia
   (is (= 1 (precedencia 'OR)))
   (is (= 2 (precedencia 'AND)))
   (is (= 3 (precedencia 'NOT)))
   (is (= 4 (precedencia '>)))
   (is (= 5 (precedencia '+)))
   (is (= 6 (precedencia '*)))
   (is (= 7 (precedencia '-u)))
   (is (= 9 (precedencia 'MID$)))   
)

(deftest test-aridad
   (is (= 0 (aridad 'THEN)))
   (is (= 1 (aridad 'SIN)))
   (is (= 2 (aridad '*)))
   (is (= 2 (aridad 'MID$)))
   (is (= 3 (aridad 'MID3$)))
   (is (= 1 (aridad '-u)))      
)

(deftest test-eliminar-cero-decimal
   (is (= 1.5 (eliminar-cero-decimal 1.5)))   
   (is (= 1.5 (eliminar-cero-decimal 1.50)))
   (is (= 1 (eliminar-cero-decimal 1.0)))
   (is (= 'A (eliminar-cero-decimal 'A)))
   (is (= 1.02 (eliminar-cero-decimal 1.020)))
   (is (= 328.02 (eliminar-cero-decimal 328.0200)))
   (is (= 0 (eliminar-cero-decimal 0.0)))
   (is (= 320 (eliminar-cero-decimal 320.0)))
)

(deftest test-eliminar-cero-entero
   (is (= nil (eliminar-cero-entero nil)))
   (is (= "A" (eliminar-cero-entero 'A)))
   (is (= "0" (eliminar-cero-entero 0)))
   (is (= "1.5" (eliminar-cero-entero 1.5)))   
   (is (= "1" (eliminar-cero-entero 1)))
   (is (= "-1" (eliminar-cero-entero -1)))
   (is (= "-1.5" (eliminar-cero-entero -1.5)))
   (is (= ".5" (eliminar-cero-entero 0.5)))
   (is (= "-.5" (eliminar-cero-entero -0.5)))
   (is (= "100.5" (eliminar-cero-entero 100.500)))  
)

(deftest test-calcular-rpn
   (is (= 3 (calcular-rpn '(1 2 +) [10 1])))
   (is (= 6 (calcular-rpn '("PRUEBA" LEN) [10 1])))
   (is (= "ESTRELLA: *" (calcular-rpn '("ESTRELLA: " "*" +) [10 1])))
   (is (= 6 (calcular-rpn '("PRUEBA" LEN) [10 1])))
   (is (= 7 (calcular-rpn '("PRUEBA" LEN 1 +) [10 1])))
   (is (= "RANCO" (calcular-rpn '("FRANCO" 2 MID$) [10 1])))
   (is (= "RANCO" (calcular-rpn '("FRANCO" 2 MID$) [10 1])))
   (is (= "1" (calcular-rpn (list "1011" 4 1 '- 1 '+ 1 'MID3$) [10 1])))
)

(deftest test-shunting-yard
   (is (= '(2 5 +) (shunting-yard '(2 + 5))))
   (is (= '("ESTRELLA: " "*" +) (shunting-yard '("ESTRELLA: " + "*"))))
   (is (= '("PRUEBA" LEN 1 +) (shunting-yard '(LEN "PRUEBA" + 1))))
   (is (= '(0 100 * INT 100 /) (shunting-yard (list 'INT (symbol "(") 0 '* 100 (symbol ")") '/ 100))))
   (is (= (list "1011" 4 1 '- 1 '+ 1 'MID3$) (shunting-yard (list 'MID3$ (symbol "(") "1011" (symbol ",") 4 '- 1 '+ 1 (symbol ",") 1 (symbol ")")))))
)

(deftest test-aplicar
   (is (= 3 (aplicar '+ 2 1 [10 1])))   
)

(deftest test-calcular-expresion
   (is (= 7 (calcular-expresion '(X + 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])))
   (is (= 34 (calcular-expresion '(X + 2 + 6 + 4 + 10) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 12}])))
   (is (= 5 (calcular-expresion '(5) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])))
   (is (= 4 (calcular-expresion (list 'LEN (symbol "(") 'X$ (symbol ")")) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}])))
   (is (= 1 (calcular-expresion '(X <= 3) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])))
   (is (= "HOLA *" (calcular-expresion '(S$ + "*") ['((10 (PRINT X))) [10 1] [] [] [] 0 '{S$ "HOLA "}])))
   (is (= 5 (calcular-expresion (list 'LEN (symbol "(") 'X$ (symbol ")") '+ 1) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}])))
)

(deftest test-leer-data
   ;Solo para ver la salida
   ;(is (= "" (leer-data '(X$) '[((1000 (DATA ALFA , BRAVO , CHARLIE , DELTA , ECHO , FOXTROT , GOLF , HOTEL)) (1010 (DATA INDIA , JULIETT , KILO , LIMA , MIKE , NOVEMBER , OSCAR , PAPA)) (1020 (DATA QUEBEC , ROMEO , SIERRA , TANGO , UNIFORM , VICTOR , WHISKEY , X-RAY)) (1030 (DATA YANKEE) (DATA ZULU ) (REM EL ULTIMO VALOR ES ZULU)) (1040 (DATA HOLA MUNDO))) [160 2] [] [[I 4 1 [130 1]] [J 6 1 [160 3]]] [ALFA BRAVO CHARLIE DELTA ECHO FOXTROT GOLF HOTEL INDIA JULIETT KILO LIMA MIKE NOVEMBER OSCAR PAPA QUEBEC ROMEO SIERRA TANGO UNIFORM VICTOR WHISKEY X-RAY HOLA MUNDO] 0 {W$ FRAN, T 4, I 1, L 6, J 1}])))
)

(run-tests)