# ALP2025-LCC

Evaluador de expresiones matematicas con diferenciacion automatica usando numeros duales.

Este proyecto fue desarrollado como trabajo final para Analisis de Lenguajes de Programacion, en la Licenciatura en Ciencias de la Computacion de la UNR. La propuesta original era construir un DSL/EDSL chico para expresar funciones matematicas y mostrar el comportamiento de los numeros duales. Luego se extendio hacia una calculadora de expresiones, pero el nucleo conceptual sigue siendo la diferenciacion automatica.

## Que hace

El programa toma expresiones escritas como texto, las transforma a un AST propio y luego las interpreta de varias formas: evaluacion escalar, evaluacion dual, pretty printing y optimizacion conservadora. El modo de usuario trabaja principalmente con la variable `x`, pero el nucleo expone funciones con entorno para dejar una base de multiples variables desde la API.

Ademas de calcular resultados, el proyecto cuida casos de dominio: division por cero, variables no definidas, potencias fuera del dominio real, funciones como `log`, `sqrt`, `arcosh`, `artanh` y discontinuidades de `tan`.

## Sintaxis soportada

Operadores:

```text
+  -  *  /  ^
```

Funciones:

```text
sin, cos, tan
sinh, cosh, tanh
arsinh, arcosh, artanh
sqrt, exp, log
```

Constantes:

```text
pi, e
```

Ejemplos:

```text
x^2 + 2*x + 1
sin(x) * exp(x)
sqrt(x^2 + 1)
log(e^x)
```

## Uso

Modo interactivo:

```powershell
cabal run ALP2025-LCC
```

Procesar un archivo:

```powershell
cabal run ALP2025-LCC -- -f examples\basico.txt
```

Formato de archivo:

```text
expresion @ valor_de_x
```

El valor de `x` puede ser un numero o una expresion constante:

```text
sin(x) @ pi/2
```

Los archivos pueden usar comentarios de linea con `--` y comentarios de bloque con `{- ... -}`.

## Tests

Comando principal:

```powershell
cabal test
```

Comando alternativo util en Windows:

```powershell
cabal exec -- runghc -isrc test\TestSuite.hs
```

La suite actual tiene 138 casos.

## Estructura del proyecto

```text
app/
  Main.hs              Despacho de argumentos del ejecutable.
  Console.hs           Interfaz interactiva por consola.

src/
  DSL.hs               API publica del lenguaje.
  Expr.hs              AST y optimizaciones conservadoras.
  Parser.hs            Parser de expresiones con Parsec.
  Evaluator.hs         Evaluacion escalar y dual.
  EvalM.hs             Monada de evaluacion con entorno y errores.
  FileReader.hs        Procesamiento de archivos.
  NumericPolicy.hs     Tolerancias y criterios numericos compartidos.
  PrettyPrinter.hs     Impresion legible de expresiones.

test/
  TestSuite.hs         Suite de tests con HUnit.

examples/
  *.txt                Casos de entrada para la calculadora.
  monada_eval.hs       Ejemplo de uso directo de EvalM.

docs/
  guia-uso.tex        Guia de uso actualizada en LaTeX.
  guia-uso.pdf        Guia de uso compilada.
  informe-final.tex    Informe final en LaTeX.
  informe-final.pdf    Informe final compilado.

Extraer antes de eentrega/
  Material auxiliar que no forma parte del entregable principal.
```

## Decisiones de diseno

### Separacion entre lenguaje e interfaz

El modulo `DSL` funciona como punto de entrada del nucleo del trabajo: reexporta el
AST, el parser, el evaluador, la monada de evaluacion y el pretty printer. El
ejecutable queda separado en `app/`, donde `Main` solo decide el modo de uso y
`Console` contiene la interaccion con el usuario. Esta division permite probar y
razonar sobre el lenguaje sin depender de la consola.

### Numeros duales

Un numero dual tiene la forma:

```text
a + b epsilon, con epsilon^2 = 0
```

Al evaluar una funcion diferenciable en `x + epsilon`, se obtiene:

```text
f(x + epsilon) = f(x) + f'(x) epsilon
```

Por eso, si se evalua una expresion usando `Dual x 1`, la parte `primal` contiene el valor de la funcion y la parte `deriv` contiene la derivada respecto de `x`.

### EvalM

`EvalM` modela dos efectos:

- lectura de un entorno de variables;
- propagacion de errores.

Su forma es:

```haskell
newtype EvalM env a = EvalM (EvalEnv env -> Either ErrorType a)
```

Esto se parece a combinar una idea de `Reader` con una idea de `Either`, pero implementado a mano para que sea claro en el contexto del trabajo.

La instancia de `Monad` permite escribir el evaluador con `return` y `>>=`, de modo que cada paso pueda consultar el entorno y propagar errores. Tambien tiene instancia `Alternative`. El operador `<|>` se usa como fallback:

```haskell
lookupVar "z" <|> lookupVar "x"
```

Si la primera busqueda falla, se intenta la segunda en el mismo entorno.

### Parser

El parser respeta esta precedencia:

```text
potencia > menos unario > multiplicacion/division > suma/resta
```

Tambien consume toda la entrada con `eof`, asi no acepta texto sobrante despues de una expresion valida.

### Pretty printer

El pretty printer minimiza parentesis, pero debe conservar el significado de la expresion. Por eso se testean casos borde como:

- potencias con base negativa;
- potencias anidadas;
- restas y divisiones anidadas a derecha;
- roundtrip `prettyPrint -> parse`.

Esta decision evita que la forma impresa de una expresion cambie su significado al volver a parsearla.

### Optimizaciones

Las optimizaciones son conservadoras. Se aplican reglas simples como:

```text
x + 0 = x
x * 1 = x
x / 1 = x
x^1 = x
2 + 3 = 5
```

No se aplican reglas que podrian ocultar errores:

```text
x / x = 1
x * 0 = 0
x^0 = 1
1^x = 1
```

Por ejemplo, si `x = 0`, entonces `x/x` debe seguir dando division por cero y `x^0` debe seguir respetando el caso `0^0`.

La invariante del optimizador es que no debe convertir una expresion que falla en
otra que parezca valida. Por eso se prefieren menos simplificaciones antes que
perder errores importantes del lenguaje.

### Criterios numericos

Las tolerancias numericas estan centralizadas en `NumericPolicy`. Esto evita que
el evaluador, el optimizador y las pruebas usen umbrales dispersos sin nombre.

### Tests de robustez

La suite no prueba solamente casos felices. Incluye dominios invalidos, tangente en puntos no definidos, potencias no finitas, valores cercanos a cero, palabras reservadas mal usadas, parentesis incompletos, archivos con comentarios, archivos sin expresiones y roundtrip entre pretty printer y parser.

## Documentacion

- `docs/informe-final.tex`: informe final en LaTeX.
- `docs/informe-final.pdf`: informe final compilado.
- `docs/guia-uso.tex`: guia de uso actualizada en LaTeX.
- `docs/guia-uso.pdf`: guia de uso compilada.
