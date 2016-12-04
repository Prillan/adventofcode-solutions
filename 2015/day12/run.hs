import Data.Aeson
import qualified Data.ByteString.Lazy as B

nums (Object o) = foldl (\acc v -> nums v ++ acc) [] o
nums (Array a) = foldl (\acc v -> nums v ++ acc) [] a
nums (Number n) = pure n
nums _ = []

process x = sum . map truncate . nums <$> decode x

main = B.readFile "input.json" >>= print . process
