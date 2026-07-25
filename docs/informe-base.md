# Informe base - Evaluador con números duales

## 1. Resumen

El proyecto implementa un lenguaje pequeño para expresar funciones matemáticas y evaluarlas en un punto. Además de calcular el valor de la función, calcula su derivada usando números duales.

La primera versión estaba centrada en una variable `x`. Luego se extendió el trabajo hacia una calculadora de expresiones, agregando más funciones, parser desde archivo, optimizaciones simples, manejo de errores y una mónada propia de evaluación.

La idea principal no es construir un CAS completo, sino mostrar cómo se puede modelar un lenguaje de expresiones matemáticas en Haskell, usando tipos algebraicos, parser combinators, type classes y mónadas.

## 2. Objetivos

Objetivos principales:

- representar expresiones matemáticas mediante un AST;
- parsear expresiones desde texto;
- evaluar expresiones en un punto;
- calcular derivadas con diferenciación automática;
- manejar errores de dominio de forma explícita;
- incorporar una mónada de evaluación vinculada con los temas de la cátedra.

Objetivos secundarios:

- mejorar la robustez del parser;
- ordenar el código para futuras extensiones;
- dejar una base parcial para múltiples variables;
- documentar las decisiones de diseño.

## 3. Representación de expresiones

Las expresiones se representan con el tipo `Expr`, definido como un tipo algebraico. Este tipo contiene literales, variables, operadores binarios y funciones unarias.

Ejemplos de constructores:

```haskell
Lit Double
Var String
Add Expr Expr
Mul Expr Expr
Pow Expr Expr
Sin Expr
Log Expr
Sqrt Expr
```

Esta representación permite recorrer la expresión por pattern matching. Esa es una ventaja importante de Haskell para implementar un DSL de este estilo: el árbol sintáctico queda explícito y las reglas de evaluación se escriben de forma declarativa.

## 4. Parser

El parser se implementa con Parsec. Se definió un lexer para reconocer operadores, identificadores, constantes y funciones reservadas.

La precedencia actual es:

```text
potencia > menos unario > multiplicación/división > suma/resta
```

Una decisión importante fue hacer que el parser consuma toda la entrada usando `eof`. Antes, una cadena como:

```text
x + 1 basura
```

podía aceptarse parcialmente. Ahora se rechaza porque sobra texto luego de una expresión válida. Esto mejora la robustez del lenguaje y evita resultados inesperados.

También se corrigió la interpretación de `-x^2`. Actualmente se parsea como:

```text
-(x^2)
```

que coincide con la convención matemática usual.

## 5. Números duales

Un número dual tiene la forma:

```text
a + bε, con ε² = 0
```

Si se evalúa una función diferenciable en `x + ε`, se obtiene:

```text
f(x + ε) = f(x) + f'(x)ε
```

En el código se representa con:

```haskell
data Dual = Dual { primal :: Double, deriv :: Double }
```

La parte `primal` almacena el valor de la función, y `deriv` almacena la derivada respecto de la variable elegida.

Las instancias `Num`, `Fractional` y `Floating` permiten escribir reglas algebraicas de forma natural. Por ejemplo, la multiplicación de duales aplica automaticamente la regla del producto:

```text
(a + bε)(c + dε) = ac + (ad + bc)ε
```

## 6. Evaluación escalar y evaluación dual

El evaluador tiene dos caminos:

- `eval`, que devuelve el valor numérico;
- `evalDual`, que devuelve valor y derivada.

Las versiones tradicionales mantienen el uso simple con la variable `x`:

```haskell
eval :: Expr -> Double -> Either ErrorType Double
evalDual :: Expr -> Double -> Either ErrorType Dual
```

Además se agregaron versiones con entorno:

```haskell
evalWithEnv :: EvalEnv Double -> Expr -> Either ErrorType Double
evalDualWithEnv :: EvalEnv Dual -> Expr -> Either ErrorType Dual
```

Esto permite evaluar expresiones con más variables desde la API, aunque el modo interactivo y el modo archivo sigan centrados en `x`.

## 7. Mónada de evaluación

Se agregó una mónada propia:

```haskell
newtype EvalM env a = EvalM (EvalEnv env -> Either ErrorType a)
```

Esta mónada modela dos efectos:

- lectura de variables desde un entorno;
- propagación de errores.

En términos de la materia, combina una idea parecida a `Reader` con una idea parecida a `Either`. No se usó un transformador de mónadas porque para el alcance del trabajo era más claro implementar el tipo propio y mostrar directamente cómo funciona.

La operación central es:

```haskell
runEvalM :: EvalEnv env -> EvalM env a -> Either ErrorType a
```

Con esto, el evaluador deja de pasar manualmente el valor de `x` por todos lados y empieza a depender de un entorno explícito.

Ejemplo:

```haskell
evalWithEnv [("x", 2), ("y", 3)] (Add (Mul (Var "x") (Var "y")) (Var "y"))
```

Resultado:

```haskell
Right 9.0
```

Para derivar respecto de `x`:

```haskell
evalDualWithEnv
  [("x", Dual 2 1), ("y", Dual 3 0)]
  (Add (Mul (Var "x") (Var "y")) (Var "y"))
```

Resultado:

```haskell
Right (Dual {primal = 9.0, deriv = 3.0})
```

## 8. Manejo de errores

Los errores se representan con:

```haskell
data ErrorType
  = DivideByZero
  | UndefinedVariable String
  | DomainError String
```

Esto permite distinguir errores de dominio, división por cero y variables no definidas.

También se agregaron funciones seguras para operaciones delicadas con números duales:

- `safeLogDual`;
- `safeSqrtDual`;
- `safeAcoshDual`;
- `safeAtanhDual`;
- `safePowDual`.

La razón es evitar que el evaluador principal dependa de funciones parciales. Por ejemplo, `log(0)` debe devolver un `DomainError`, no cortar la ejecución con una excepción.

## 9. Optimización de expresiones

El módulo `Expr` incluye una función `optimize` con reglas algebraicas simples:

```text
x + 0 = x
x * 1 = x
x / 1 = x
x^1 = x
2 + 3 = 5
2^3 = 8
```

Estas reglas no buscan hacer álgebra simbólica completa. Sirven para limpiar expresiones comunes antes de evaluar y para mostrar una transformación básica sobre el AST.

La optimización es conservadora: no aplica reglas como `x/x = 1`, `x*0 = 0`, `x^0 = 1` o `1^x = 1` cuando podrían ocultar errores del evaluador. Por ejemplo, si `x = 0`, entonces `x/x` debe seguir dando división por cero y `x^0` debe seguir respetando el caso `0^0`.

## 10. Procesamiento de archivos

El modo archivo usa líneas del tipo:

```text
expresion @ valor
```

Ejemplo:

```text
sin(x) @ pi/2
x^2 + 2*x + 1 @ 3
```

El valor a la derecha de `@` puede ser un número o una expresión constante. Esto hace más cómodo usar `pi`, `e` o combinaciones como `pi/2`.

## 11. Tests

La suite de tests cubre:

- evaluación básica;
- evaluación dual;
- parser;
- funciones trigonométricas e hiperbólicas;
- constantes;
- casos de dominio;
- potencias con bases negativas;
- optimizaciones;
- uso de la mónada `EvalM`;
- procesamiento de valores constantes en archivos.

La validación actual se realizó con:

```powershell
cabal exec -- runghc -isrc test\TestSuite.hs
```

Resultado:

```text
125 casos, 0 errores, 0 fallos
```

La version final del informe se dejo en `docs/informe-final.tex`, para poder compilarlo aparte a PDF.

## 12. Comparación con un CAS simbólico

Un CAS simbólico suele implementar más funcionalidades, como derivación simbólica, integración y simplificaciones avanzadas.

En este proyecto se tomaron decisiones más acotadas. El pretty printer se prueba con casos de roundtrip: se imprime una expresión, se vuelve a parsear y se compara su evaluación. Además, `EvalM` incorpora una instancia `Alternative`, útil para expresar fallbacks internos sin cambiar el lenguaje de usuario.

Este proyecto toma una dirección más acotada:

- se enfoca en diferenciación automática;
- evita convertirse en un CAS completo;
- usa números duales como técnica central;
- usa mónadas para ordenar errores y entorno.

Esta diferencia de alcance es importante. Un enfoque de CAS completo haría crecer mucho el trabajo y desviaría el eje original presentado.

## 13. Limitaciones

Limitaciones actuales:

- no hay derivación simbólica general;
- no hay integración;
- las simplificaciones son básicas;
- el modo interactivo no permite elegir varias variables;
- el soporte multivariable existe parcialmente en la API, no como lenguaje completo de usuario;
- algunas instancias de `Dual` siguen teniendo operaciones parciales, aunque el evaluador principal usa funciones seguras.

## 14. Trabajo futuro

Posibles extensiones:

- permitir elegir variable de derivación desde entrada de usuario;
- extender el formato de archivo para entornos multivariables;
- mejorar mensajes de error;
- agregar más funciones matemáticas;
- generar el informe final en PDF desde esta base editable;
- agregar tests de ejemplos como parte automatizada de la suite.

## 15. Conclusión

El proyecto muestra cómo representar y evaluar un lenguaje de expresiones matemáticas en Haskell. La diferenciación automática con números duales permite calcular valor y derivada en una sola pasada de evaluación.

La incorporación de `EvalM` mejora la estructura del evaluador porque separa la lógica del dominio de los efectos de error y entorno. Esto vuelve el código más robusto y más fácil de defender desde los contenidos de la materia.

El alcance actual es razonable para el trabajo: no intenta resolver todo el álgebra simbólica, sino presentar un DSL chico, testeado y con decisiones de diseño claras.
