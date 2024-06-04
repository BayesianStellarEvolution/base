module Types.DistanceMeasures (Parallax (..)) where

import Types.Internal


{-
Note [Distance Moduli]
~~~~~~~~~~~~~~~~~~~~~~

one other thing to remember is that parallax maps
to the so-called *true distance modulus*, indicated as (m-M)o, which
is not something we use in our code.  Instead BASE-9 uses the *observed
distance modulus*, indicated by (m-M)V.  The difference in the two is
due to the absorption and the transformation is
    (m-M)V = (m-M)o + Av .
-}

newtype Parallax = MkParallax { unParallax :: NonNegative }
        deriving (Show, Eq, Ord)

