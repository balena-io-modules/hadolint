module Hadolint.Formatter (formatCheck, formatError) where

import Data.Char (isSpace)
import Control.Monad
import Hadolint.Rules
import Hadolint.Syntax
import Text.Parsec.Error (Message, ParseError, messageString, errorPos, errorMessages, showErrorMessages)
import Text.Parsec.Pos

formatError :: ParseError -> String
formatError err =  posPart ++ stripNewlines msgPart
  where
    pos = errorPos err
    posPart = sourceName pos ++ ":" ++ show (sourceLine pos) ++ ":" ++ show (sourceColumn pos)
    msgPart = showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (errorMessages err)
    stripNewlines = map (\c -> if c =='\n' then ' ' else c)


formatCheck :: Maybe String -> Check -> String
formatCheck filename (Check metadata source linenumber _) = formatPos displayName linenumber ++ code metadata ++ " " ++ message metadata
  where
    displayName = case filename of
                    (Just name) -> name
                    Nothing -> source


formatPos :: Filename -> Linenumber -> String
formatPos source linenumber = if linenumber >= 0
                              then source ++ ":" ++ show linenumber ++ " "
                              else source ++ " "
