module Musicology.Pitch.Internal
  ( parseInt
  , parseInt'
  , munchChar
  , munchChar1
  )
where

import qualified Text.ParserCombinators.ReadP  as R
import           Data.Char                      ( isDigit )

parseInt :: R.ReadP Int
parseInt = do
  sign <- R.option "" $ R.string "-"
  dgts <- R.munch1 isDigit
  pure $ read $ sign <> dgts

parseInt' :: R.ReadP Int
parseInt' = read <$> R.munch isDigit

munchChar :: Char -> R.ReadP String
munchChar c = R.munch (== c)

munchChar1 :: Char -> R.ReadP String
munchChar1 c = R.munch1 (== c)
