module NumericPolicy
  ( epsilonNumerico
  , epsilonEntero
  , esCasiCero
  , esEntero
  , esFinito
  ) where

-- Tolerancia para divisiones y singularidades numericas cerca de cero.
epsilonNumerico :: Double
epsilonNumerico = 1e-15

-- Tolerancia para decidir si un exponente Double representa un entero.
epsilonEntero :: Double
epsilonEntero = 1e-10

esCasiCero :: Double -> Bool
esCasiCero x = abs x < epsilonNumerico

esEntero :: Double -> Bool
esEntero x =
  let redondeado = fromIntegral (round x :: Integer)
  in if redondeado == 0
     -- Cerca de cero pedimos igualdad exacta para no confundir exponentes chicos.
     then x == 0
     else abs (x - redondeado) < epsilonEntero

esFinito :: Double -> Bool
esFinito x = not (isNaN x || isInfinite x)
