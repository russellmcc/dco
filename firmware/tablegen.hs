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

maxScaledPeriod = 0x10000

-- if this is 1200, no correction will occur.
-- set this to what the apparent cents per octave is
-- with this set to 1200.  The correction will occur.
centsPerOctaveCorrection = 1185

periodForFreq ∷ Double → Double
periodForFreq f = cpuFreq / (doCorrect f)
  where
    doCorrect f = 2 ** ((logBase 2 f) * (1200 / centsPerOctaveCorrection))

periodRegsForFreq ∷ Double → (Integer, Integer, Double)
periodRegsForFreq f = (hi, lo, eff)
  where
    p = round $ periodForFreq f
    cDiv x y = ceiling $ ((/) `on` fromIntegral) x y
    hi' = if (p `mod` maxScaledPeriod) == 0 then (p `quot` maxScaledPeriod) - 1 else p `cDiv` maxScaledPeriod
    hi'' = max 2 hi'
    lo = (round (((/) `on` fromIntegral) p hi'')) - 1
    hi = hi'' - 1
    eff = cpuFreq / (((*) `on` fromIntegral) (hi + 1) (lo + 1))

-- in cents
freqError f = (1200 * abs (logBase 2 (effF / f)), hi, lo, f)
  where
    (hi, lo, effF) = periodRegsForFreq f

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
               "\nconst uint16_t kHi[] PROGMEM = " ++ (showCList $ snd' <$> l) ++ ";\n" ++
               "\nconst uint16_t kLo[] PROGMEM = " ++ (showCList $ thd' <$> l) ++ ";\n"

allData f = j (amp f) (periodRegsForFreq f) (freqError f)
  where j a (hi, lo, _) e = (a, hi, lo, e)

-- from schem
voltageScale = 33/51
voltage n = (fromIntegral n / 4095) * 3.3 / voltageScale
baseFreq = 70
freq n = baseFreq * (2 ** (voltage n))

main ∷ IO ()
main = putStr (printCCode $ allData <$> freq <$> [0..4095])