import qualified Data.Map as Map
import Data.Maybe
import Control.Applicative
import System.IO

exps2 = map (round . (2**)) [0,1..8]
expsFuncs10 = map ((*) . round . (10**)) [8,7..0]

digitsInitial =
  [([0,0,0,0,0,1,0,0,1], 1)
  ,([0,1,0,0,1,1,1,1,0], 2)
  ,([0,1,0,0,1,1,0,1,1], 3)
  ,([0,0,0,1,1,1,0,0,1], 4)
  ,([0,1,0,1,1,0,0,1,1], 5)
  ,([0,1,0,1,1,0,1,1,1], 6)
  ,([0,1,0,0,0,1,0,0,1], 7)
  ,([0,1,0,1,1,1,1,1,1], 8)
  ,([0,1,0,1,1,1,0,1,1], 9)
  ]

digits = Map.fromList . map (\(key, value) -> (sum $ zipWith (*) key exps2, value)) $ digitsInitial

chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n xs = ys : chunk n zs
  where (ys,zs) = splitAt n xs

morphLine :: String -> [[Int]]
morphLine = map (map (\x -> if x == ' ' then 0 else 1)) . chunk 3

decode :: [Int] -> Maybe Int
decode digitsCodes =
  let
    decodedDigits    = map (`Map.lookup` digits) digitsCodes
    multipliedDigits = zipWith fmap expsFuncs10 decodedDigits
  in foldl1 (\acc x -> (+) <$> acc <*> x) multipliedDigits
  
-- slice text on files and group them by 3 (last empty line removed with init)
sliceText = map init . chunk 4 . lines
-- replace non-empty symbol with 1 and space with 0 
replaceSymbols = return . map morphLine
-- form single array for each digit 
formDigits = return . foldl1 (zipWith (++))
-- zips binary 0 and 1 with exponents of 2
zipWithExponents = return . map (zipWith (*) exps2)
-- sum digit array and decode all digits 
decodeDigits = return . decode . map sum

main = do
  text <- readFile "kata.test"
  let
    initialLines = sliceText text
    numbers = initialLines >>= replaceSymbols >>= formDigits >>= zipWithExponents >>= decodeDigits
  mapM_ print numbers

