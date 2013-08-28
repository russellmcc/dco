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

--prescales = [1,2,4,8,256,1024]
prescales = [8]
prescaleStr p = "TC_CLKSEL_DIV" ++ show p ++ "_gc"

newtype PlainString = PlainString String deriving (Ord, Eq)
instance Show PlainString where
  show (PlainString s) = s

maxScaledPeriod = 0x10000

ratDiv ∷ Integer → Integer → Rational
ratDiv = (/) `on` fromInteger

periodForFreq ∷ Double → Double
periodForFreq f = cpuFreq / f

scaledPeriod ∷ Double → Integer → (Integer, Int)
scaledPeriod p s = (s, round $ (p / (fromIntegral s)))

scaledPeriods f = scaledPeriod <$> pure (periodForFreq f) <*> prescales

bestScaledPeriod = second (-1 +) . head . mfilter ((maxScaledPeriod >) . snd) . scaledPeriods

-- in cents
freqError f = (1200 * abs (logBase 2 (effF / f)), effF, f, bestScaledPeriod f)
  where
    effF = cpuFreq / (fromIntegral (snd $ bestScaledPeriod f)) / (fromIntegral (fst $ bestScaledPeriod f))
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
    minAmp = 192 -- we can't start from the bottom of the range because DAC isn't linear there
    maxAmp = 4095 -- go to the top of the dac range

fst' (a, _, _, _) = a
snd' (_, b, _, _) = b
thd' (_, _, c, _) = c
fth' (_, _, _, d) = d

showCList ∷ (Show a) ⇒ [a] → String
showCList l = "{" ++ (help l) ++ "}"
  where
    help (x:y:xs) = show x ++ "," ++ help (y:xs)
    help (y:[]) = show y

printCCode l = "// max error: " ++ (show (maximum (fth' <$> l))) ++
               "\nconst uint16_t kAmps[] PROGMEM = " ++ (showCList $ fst' <$> l) ++ ";\n" ++
               "\nconst uint8_t kScales[] PROGMEM = " ++ (showCList $ snd' <$> l) ++ ";\n" ++
               "\nconst uint16_t kPeriods[] PROGMEM = " ++ (showCList $ thd' <$> l) ++ ";\n"

allData f = j (amp f) (bestScaledPeriod f) (freqError f)
  where j a (p, s) e = (a, prescaleStr p, s, e)

-- from schem
voltageScale = 33/51
voltage n = (fromIntegral n / 4095) * 3.3 / voltageScale
baseFreq = 70
freq n = baseFreq * (2 ** (voltage n))

main ∷ IO ()
main = putStr (printCCode $ allData <$> freq <$> [0..4095])