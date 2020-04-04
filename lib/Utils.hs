{-# LANGUAGE TemplateHaskellQuotes, OverloadedStrings #-}
module Utils where

import Language.Haskell.TH.Quote
import Data.Text (Text, pack)
import Data.Char
import Data.List
import qualified Data.Text as T
import qualified Data.String.Interpolate as I

tshow :: Show a => a -> Text
tshow = pack . show

unindent :: String -> String
unindent s = intercalate "\n" . reverse . dropWhile (all isSpace) $ reverse stripped
  where ls = dropWhile (all isSpace) $ lines s
        minIndent = minimum $ map (length . takeWhile isSpace) $ filter (not . null) ls
        stripped = map (\x -> if null x then x else drop minIndent x) ls


interp :: QuasiQuoter
interp = I.i{quoteExp = quoteExp I.i . unindent }

interp' :: QuasiQuoter
interp' = I.i

indentLang :: Word -> Word -> Text -> Text
indentLang indentSize n s = T.intercalate "\n" $ case T.lines s of
  (x:xs) -> x : map (T.replicate (fromIntegral $ n*indentSize) " " <>) xs
  [] -> []
