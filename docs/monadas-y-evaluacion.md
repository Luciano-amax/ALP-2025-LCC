# Monadas y evaluacion

## Motivacion

El evaluador original usaba `Either ErrorType a` para representar computos que podian fallar. Esa eleccion era correcta, pero dejaba mezcladas dos responsabilidades:

- propagar errores de dominio, division por cero o variables no definidas;
- decidir de donde salen los valores asociados a las variables.

Para ordenar esa parte del trabajo se agrego una monada propia, `EvalM`, inspirada en las monadas vistas en la materia para modelar efectos computacionales.

## La monada `EvalM`

El tipo principal es:

```haskell
newtype EvalM env a = EvalM (EvalEnv env -> Either ErrorType a)
```

Conceptualmente, una computacion `EvalM env a` es una evaluacion que:

- lee un entorno de variables;
- puede fallar con un `ErrorType`;
- si no falla, produce un valor de tipo `a`.

Esto combina dos efectos frecuentes:

- un efecto de lectura de contexto, similar a una monada `Reader`;
- un efecto de error, similar a `Either`.

La funcion que ejecuta la computacion es:

```haskell
runEvalM :: EvalEnv env -> EvalM env a -> Either ErrorType a
```

El entorno se representa como una lista de pares:

```haskell
type EvalEnv a = [(String, a)]
```

De esta manera, `lookupVar` busca una variable en el entorno y devuelve `UndefinedVariable` si no esta definida.

## Relacion con el evaluador

Las funciones publicas tradicionales se mantienen:

```haskell
eval :: Expr -> Double -> Either ErrorType Double
evalDual :: Expr -> Double -> Either ErrorType Dual
```

Internamente, ambas se expresan usando versiones con entorno:

```haskell
evalWithEnv :: EvalEnv Double -> Expr -> Either ErrorType Double
evalDualWithEnv :: EvalEnv Dual -> Expr -> Either ErrorType Dual
```

Esto permite conservar el comportamiento original, donde `x` es la variable principal, pero tambien deja preparado el codigo para evaluar expresiones con mas variables.

Por ejemplo, para evaluar `x*y + y` con `x = 2` e `y = 3`:

```haskell
evalWithEnv [("x", 2), ("y", 3)] (Add (Mul (Var "x") (Var "y")) (Var "y"))
```

El resultado es:

```haskell
Right 9.0
```

## Derivacion con entorno

En el caso dual, el entorno guarda numeros duales. Para derivar respecto de `x`, se asigna derivada `1` a `x` y derivada `0` a las variables que se consideran constantes.

```haskell
evalDualWithEnv
  [("x", Dual 2 1), ("y", Dual 3 0)]
  (Add (Mul (Var "x") (Var "y")) (Var "y"))
```

El resultado es:

```haskell
Right (Dual {primal = 9.0, deriv = 3.0})
```

La parte primal corresponde al valor de la funcion, y la parte dual corresponde a la derivada respecto de `x`.

## Manejo seguro de errores

Tambien se agregaron operaciones seguras para casos delicados de numeros duales:

- `safeLogDual`;
- `safeSqrtDual`;
- `safeAcoshDual`;
- `safeAtanhDual`;
- `safePowDual`.

Estas funciones devuelven errores dentro de `EvalM` en lugar de depender de excepciones parciales. Por ejemplo, `log(0)` o `sqrt(-1)` producen `Left (DomainError ...)`.

Las instancias `Num`, `Fractional` y `Floating` de `Dual` siguen existiendo porque simplifican la implementacion de las reglas algebraicas. Sin embargo, el camino principal del evaluador usa las operaciones seguras cuando hay restricciones de dominio.

## Alternativas de evaluacion

Tambien se agrego una instancia `Alternative` para `EvalM`. La idea es poder expresar una computacion con respaldo:

```haskell
lookupVar "z" <|> lookupVar "x"
```

Si la primera busqueda falla, se intenta la segunda en el mismo entorno. Esto no cambia el modo de uso del programa, pero deja una herramienta chica y prolija para futuras validaciones internas.

## Alcance

Esta mejora no convierte al lenguaje en un sistema completo de multiples variables. El modo interactivo y el formato de archivo siguen enfocados en evaluar respecto de `x`.

Lo que si aporta es una base mas robusta:

- los errores quedan modelados de forma uniforme;
- el entorno de variables queda explicitado;
- la extension a multiples variables queda mejor encaminada;
- el uso de monadas queda integrado al diseño del evaluador, no solo mencionado como detalle de implementacion.
