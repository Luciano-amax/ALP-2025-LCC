# ALP2025-LCC

Evaluador de expresiones matematicas con diferenciacion automatica usando numeros duales.

Este proyecto fue desarrollado como trabajo final para Analisis de Lenguajes de Programacion, en la Licenciatura en Ciencias de la Computacion de la UNR. La propuesta original era construir un DSL/EDSL chico para expresar funciones matematicas y mostrar el comportamiento de los numeros duales. Luego se extendio hacia una calculadora de expresiones, pero el nucleo conceptual sigue siendo la diferenciacion automatica.

## Que hace

- Parsea expresiones matematicas desde texto.
- Evalua expresiones en un valor de `x`.
- Calcula derivadas automaticamente usando numeros duales.
- Procesa archivos con varias expresiones.
- Maneja errores de dominio, division por cero y variables no definidas.
- Aplica optimizaciones algebraicas conservadoras.
- Expone una API con entorno de variables mediante `EvalM`.

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

La suite actual tiene 103 casos.

## Estructura del proyecto

```text
app/
  Main.hs              Entrada del programa.

src/
  Expr.hs              AST y optimizaciones conservadoras.
  Parser.hs            Parser de expresiones con Parsec.
  Evaluator.hs         Evaluacion escalar y dual.
  EvalM.hs             Monada de evaluacion con entorno y errores.
  FileReader.hs        Procesamiento de archivos.
  PrettyPrinter.hs     Impresion legible de expresiones.

test/
  TestSuite.hs         Suite de tests con HUnit.

examples/
  *.txt                Casos de entrada para la calculadora.
  monada_eval.hs       Ejemplo de uso directo de EvalM.

docs/
  informe-final.tex    Informe final en LaTeX.
  monadas-y-evaluacion.md
  informe-base.md
  *.pdf                Guia e informe previos.
```

## Decisiones de diseno

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

Tambien tiene instancia `Alternative`. El operador `<|>` se usa como fallback:

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
- roundtrip `prettyPrint -> parse -> eval`.

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

## Para defender el trabajo

Puntos importantes:

- El proyecto no es un CAS completo. No hace derivacion simbolica general ni integracion.
- La derivada se calcula por diferenciacion automatica con numeros duales.
- La calculadora es una extension del DSL original, no el eje teorico principal.
- `EvalM` se incorpora para ordenar entorno y errores, y para mostrar uso real de monadas.
- El soporte multivariable existe como base en la API (`evalWithEnv`, `evalDualWithEnv`), pero el modo de usuario sigue centrado en `x`.
- Las optimizaciones son conservadoras para preservar errores de dominio.
- El proyecto no toma el alcance de un CAS simbolico: se mantiene centrado en numeros duales y diferenciacion automatica.

## Documentacion

- `docs/informe-final.tex`: informe final en LaTeX.
- `docs/monadas-y-evaluacion.md`: explicacion especifica de `EvalM`.
- `docs/informe-base.md`: version base en Markdown.
- `docs/Dual Numbers - Guide.pdf`: guia previa.
- `docs/Dual Numbers - Informe.pdf`: informe previo.
