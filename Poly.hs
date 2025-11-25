module Poly ( Poly       -- The type of polymomials
                         -- Instances: Eq, Show

            -- Main API
            , listToPoly -- :: [Int] -> Poly
            , polyToList -- :: Poly -> [Int]
            , evalPoly   -- :: Int -> Poly -> Int
            , addPoly    -- :: Poly -> Poly -> Poly
            , mulPoly    -- :: Poly -> Poly -> Poly
            , negatePoly -- :: Poly -> Poly
            , minusPoly  -- :: Poly -> Poly

            -- Examples, for testing your functions in `cabal repl`
            , eg1, eg2, eg3 -- :: Poly

            -- Pretty printing `Poly`s, for debugging in `cabal repl`
            -- Use 'show' instead to see the list representation
            , printPoly  -- :: Poly -> IO ()
            ) where -- DO NOT CHANGE THIS EXPORT LIST!

---------------------------
-- Debugging conveniences
---------------------------

-- Try loading ghci by running `cabal repl` then running `printPoly eg1`

-- Examples of polynomials
eg1, eg2, eg3 :: Poly
eg1 = listToPoly [1,2,3] -- 'x^2 + 2x + 3'

eg2 = listToPoly [1,0] -- '1x + 0', i.e. 'x'

eg3 = listToPoly [1,1] -- '1x + 1', i.e. 'x + 1'

-- Pretty printing `Poly`s, for debugging in `cabal repl`
-- Use 'show' instead to see the list representation
printPoly :: Poly -> IO ()
printPoly p = putStrLn (prettyPoly p)

----------------------------------------------
-- Poly interface/API
-- These functions can be used in Simplify.hs
----------------------------------------------

-- | Convert a list to a polynomial.
listToPoly :: [Int] -> Poly
listToPoly = poly . reverse

-- | Convert a polynomial to a list representation.
-- @x² + 2x + 3@ would be represented by @[1,2,3]@.
polyToList :: Poly -> [Int]
polyToList (Poly ns) = reverse ns

-- | Evaluate a polynomial, given a value for x.
evalPoly :: Int -> Poly -> Int
evalPoly x (Poly cs) = sum $ zipWith (*) cs powersOfx
         where powersOfx = map (x^) [0..]

-- | Addition for polynomials.
addPoly :: Poly -> Poly -> Poly
addPoly (Poly p1) (Poly p2) =
   poly $ zipWith (+) (pad l1 p1) (pad l2 p2) -- Add the corresponding coefficients of the two polynomials.
     where pad lp p = p ++ replicate (maxlen - lp) 0 -- Make both lists the same length by right-padding the shorter one with zeroes.
           (l1, l2, maxlen) = (length p1, length p2, max l1 l2) -- Find out the highest power of x in the two polynomials.

-- | Multiplication for polynomials.
mulPoly :: Poly -> Poly -> Poly
mulPoly (Poly p1) (Poly p2) = poly $ mul p1 p2
  where
      -- Multiply each term of the first polynomial with the second polynomial, and add the results together.
      mul [] p     = []
      mul (n:ns) p =  (n `times` p) `plus` timesX (ns `mul` p)
      timesX  p = 0:p
      times n p = map (n*) p
      plus p1 p2 = reverse . polyToList $ Poly p1 `addPoly` Poly p2

-- | Negating polynomials
negatePoly :: Poly -> Poly
negatePoly = listToPoly . map negate . polyToList

-- | Subtracting polynomials.
minusPoly :: Poly -> Poly -> Poly
minusPoly p1 p2 = p1 `addPoly` (negatePoly p2)

--------------------------------------------------
-- Everything after this point is not exported,
-- so don't try to use any of it in Simplify.hs!
--------------------------------------------------

-- | Remove zeros from little-endian list (list of coefficients ordered least significant power first) if they occur at the end of
-- the list.
poly :: [Int] -> Poly
poly = Poly . reverse . strip . reverse

strip :: [Int] -> [Int]
strip = dropWhile (== 0)

-- | A type representing polynomials.
-- | DON'T USE THE `Poly` CONSTRUCTOR DIRECTLY: use `listToPoly` and `polyToList` instead
newtype Poly = Poly [Int]
  deriving (Eq)

instance Show Poly where
  show p = "listToPoly " ++ show (polyToList p)

-- | Show polynomials with Unicode characters.
prettyPoly :: Poly -> String
prettyPoly (Poly ns) =
      showParts . reverse . filter (\(n,_) -> n /= 0) $ zip ns powers
    where showParts []           = show 0
          showParts ((n,p):rest) = showNum n p ++ concatMap showRest rest

          showNum n ""   = show n -- Show the constant term.
          showNum (-1) p = "-" ++ p -- If a coefficient is -1, do not display the 1; e.g. -x instead of -1x.
          showNum 1 p    = p -- If a coefficient is 1, do not display it.
          showNum n p    = show n ++ p

          showRest (n,p) | n < 0     = " - " ++ showNum (negate n) p
                         | otherwise = " + " ++ showNum n          p

          -- Map the powers of x to Unicode characters. Note that only powers up to 9 have corresponding
          -- Unicode characters.
          powers =
            prettyOnes ++ map (\n -> "x^" ++ show n ) [length prettyOnes ..]
              where
                prettyOnes = ["", "x"]
                          ++ ["x\178", "x\179"]
                          ++ map (\c -> 'x':c:[]) ['\8308'..'\8313']
