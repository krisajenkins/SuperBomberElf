-- TODO Take this from Kashmir instead.
module AesonUtils (dropPrefixJSONOptions) where

import           Data.Aeson.Types
import           Data.Char

dropPrefixJSONOptions :: String -> Options
dropPrefixJSONOptions s =
  defaultOptions {fieldLabelModifier =
                    lowerFirst .
                    drop (length s)}

lowerFirst :: String -> String
lowerFirst [] = []
lowerFirst (x:xs) = toLower x : xs
