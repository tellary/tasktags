module ZonedTime where

import           Control.Applicative      (empty)
import           Data.Aeson               (FromJSON (parseJSON),
                                           ToJSON (toJSON), Value (String))
import qualified Data.Text                as T
import           Data.Time                (ZonedTime)
import           Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)

instance FromJSON ZonedTime where
  parseJSON (String t) = iso8601ParseM . T.unpack $ t
  parseJSON _          = empty

instance ToJSON ZonedTime where
  toJSON = String . T.pack . iso8601Show
