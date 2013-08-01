{-# LANGUAGE UnicodeSyntax #-}

import Control.Applicative
import Control.Monad
import Data.Function
import Control.Arrow

-- The purpose of this module is to create a table for the DCO tuning.
-- tuning consists of three components - the amplitude, the period, and the
-- scaling factor.

-- this is the CPU frequency
cpuFreq ∷ Double
cpuFreq = 32000000

prescales = [1,2,4,8,256,1024]
prescaleStr p = "TC_CLKSEL_DIV" ++ show p ++ "_gc"

newtype PlainString = PlainString String
instance Show PlainString where
  show (PlainString s) = s

maxScaledPeriod = 0x10000

ratDiv ∷ Integer → Integer → Rational
ratDiv = (/) `on` fromInteger

periodForFreq ∷ Double → Integer
periodForFreq f = round $ cpuFreq / f

scaledPeriod ∷ Integer → Integer → (PlainString, Int)
scaledPeriod p s = (PlainString $ prescaleStr s, round $  ratDiv p s)

scaledPeriods f = scaledPeriod <$> pure (periodForFreq f) <*> prescales

bestScaledPeriod = second (-1 +) . head . mfilter ((maxScaledPeriod >) . snd) . scaledPeriods

-- just use linear interpolation for the amplitude.
amp ∷ Double → Int
amp = round . amp'
  where
    amp' f
      | f < minAmpFreq = minAmp
      | f > maxAmpFreq = maxAmp
      | otherwise = (f - minAmpFreq) / (maxAmpFreq - minAmpFreq) * (maxAmp - minAmp) + minAmp
    minAmpFreq = 50
    maxAmpFreq = 5000
    minAmp = 12
    maxAmp = 255

fst' (a, _, _) = a
snd' (_, b, _) = b
thd' (_, _, c) = c

showCList ∷ (Show a) ⇒ [a] → String
showCList l = "{" ++ (help l) ++ "}"
  where
    help (x:y:xs) = show x ++ "," ++ help (y:xs)
    help (y:[]) = show y

printCCode l = "const uint8_t kAmps[] = " ++ (showCList $ fst' <$> l) ++ ";\n" ++
               "const uint8_t kScales[] = " ++ (showCList $ snd' <$> l) ++ ";\n" ++
               "const uint16_t kPeriods[] = " ++ (showCList $ thd' <$> l) ++ ";\n"

allData f = j (amp f) $ bestScaledPeriod f
  where j a (p, s) = (a, p, s)

voltage n = (fromIntegral n / 4095) * 5
baseFreq = 50
freq n = baseFreq * (2 ** (voltage n))

main ∷ IO ()
main = putStr (printCCode $ allData <$> freq <$> [0..4095])