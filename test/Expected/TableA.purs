-- AUTO GENERATED CODE

module Test.TableA where

import Droplet.Language
import Type.Proxy (Proxy(..))
import Data.Date (Date)
import Data.DateTime (DateTime)

type TableA =
    ( aColumn1 :: String
    , aColumn2 :: Maybe String
    , aColumn3 :: Maybe DateTime
    , aColumn4 :: Default Date
    , id :: Auto Int
    )

tableA :: Table "table_a" TableA
tableA = Table

_aColumn1 :: Proxy "a_column1"
_aColumn1 = Proxy

_aColumn2 :: Proxy "a_column2"
_aColumn2 = Proxy

_aColumn3 :: Proxy "a_column3"
_aColumn3 = Proxy

_aColumn4 :: Proxy "a_column4"
_aColumn4 = Proxy

_id :: Proxy "id"
_id = Proxy
