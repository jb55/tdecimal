{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

module Data.TDecimal where


import Data.List (find)
import Data.Maybe (fromJust)
import Data.Set (singleton, deleteFindMin, insert)
import Data.Ratio (Ratio, numerator, denominator, (%))


newtype TDec = TDec (Int, Rational, Rational)

instance Show TDec where
    show (TDec (p, v, r)) = showR p v

instance Num TDec where
    (+) = liftT2 (+)
    (*) = liftT2 (*)
    signum (TDec (p, v1, r1))      = tdec p (signum (v1+r1))
    abs (TDec (p, v1, r1))         = tdec p (abs (v1+r1))
    fromInteger int                = tdec tdefprec (fromInteger int)
    negate      (TDec (p, v1, r1)) = tdec p (negate (v1+r1))

instance Fractional TDec where
    (/)          = liftT2 (/)
    fromRational = td

tdefprec :: Int
tdefprec = 4

tprec :: TDec -> Int
tprec (TDec (p, _, _)) = p

tdecs :: TDec -> TDec -> Rational -> TDec
tdecs t1 t2 = tdec (tprecs t1 t2)

tprecs :: TDec -> TDec -> Int
tprecs t1 t2 = tprec t1 `max` tprec t2

liftT2 :: (Rational -> Rational -> Rational) -> TDec -> TDec -> TDec
liftT2 f t1 t2 = tdecs t1 t2 (tfrac t1 `f` tfrac t2)

-- | return a TDec only containing the remainder
trem :: TDec -> Rational
trem (TDec (_, _, r)) = r

-- | drop the remainder
ttrunc :: TDec -> TDec
ttrunc (TDec (p, w,_)) = tdec p w

tfrac :: TDec -> Rational
tfrac (TDec (_, w, r)) = w+r

round_ :: Int -> Rational -> Rational
round_ places frac = (fromInteger . floor) (frac * (10^places)) / 10.0^^places

tround :: Int -> TDec -> TDec
tround places dec =
    let
        frac       = tfrac dec
        rounded    = round_ places frac
        roundedOff = frac - rounded
    in TDec (places, rounded, roundedOff)

tdec :: Int -> Rational -> TDec
tdec places r = tround places (TDec (places, r, 0))

td :: Rational -> TDec
td = tdec 4

showR :: Integral a => Int -> Ratio a -> String
showR n r =
    let d = round (abs r * 10^n)
        s = show d
        s' = replicate (n - length s + 1) '0' ++ s
        (h, f) = splitAt (length s' - n) s'
    in
      (if r < 0 then "-" else "") ++ h ++
      (if n /= 0 then "." ++ f else "")
