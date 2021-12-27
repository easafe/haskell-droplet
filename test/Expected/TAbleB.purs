-- AUTO GENERATED CODE

module Definition.TAbleB where

import Droplet.Language
import Type.Proxy (Proxy(..))
import Data.DateTime (DateTime)

type TAbleB =
    ( column1 :: Maybe Int
    , column2 :: Boolean
    , column3 :: Column DateTime (Constraint "unique_column" Unique /\ Constraint "default_column" Default)
    , id :: Column Int (PrimaryKey /\ Identity)
    )

tAbleB :: Table "t_able_b" TAbleB
tAbleB = Table

_column1 :: Proxy "column1"
_column1 = Proxy

_column2 :: Proxy "column2"
_column2 = Proxy

_column3 :: Proxy "column3"
_column3 = Proxy

_id :: Proxy "id"
_id = Proxy
