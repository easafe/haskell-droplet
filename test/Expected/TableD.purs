-- AUTO GENERATED CODE

module Definition.TableD where

import Droplet.Language
import Type.Proxy (Proxy(..))
import Data.DateTime (DateTime)

type TableD =
    ( iD :: Column Int (Identity /\ Constraint (Composite "pk_column") PrimaryKey)
    , fkColumn :: Column Int (ForeignKey "id" TableATable)
    , fk_Column :: Column Int (ForeignKey "id" TableATable)
    )

tableD :: Table "TableD" Tablec
tableD = Table

_id :: Proxy "id"
_id = Proxy

_fkColumn :: Proxy "fkColumn"
_fkColumn = Proxy

_fk_column :: Proxy "fk_column"
_fk_column = Proxy
