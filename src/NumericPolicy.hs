module NumericPolicy
  ( epsilonNumerico
  , epsilonEntero
  , esCasiCero
  , esEntero
  , esFinito
  ) where

epsilonNumerico :: Double
epsilonNumerico = 1e-15

epsilonEntero :: Double
epsilonEntero = 1e-10

esCasiCero :: Double -> Bool
esCasiCero x = abs x < epsilonNumerico

esEntero :: Double -> Bool
esEntero x =
  let redondeado = fromIntegral (round x :: Integer)
  in if redondeado == 0
     then x == 0
     else abs (x - redondeado) < epsilonEntero

esFinito :: Double -> Bool
esFinito x = not (isNaN x || isInfinite x)
