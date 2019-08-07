module GenericsSpec where

import Control.Monad.Reader
import Test.Hspec hiding (Arg)
import Text.PrettyPrint.HughesPJ (Doc)
import Weft.Generics.AllTypes
import Weft.Generics.EmptyQuery
import Weft.Generics.Hydrate
import Weft.Generics.PprQuery
import Weft.Generics.PprSchema
import Weft.Generics.QueryParser
import Weft.Generics.Resolve
import Weft.Generics.Schema
import Weft.Types


newtype Id = Id String deriving (Generic, Show, Eq, Ord)
newtype Name = Name String deriving (Generic, Show, Eq, Ord)

data User ts = User
  { userId         :: Magic ts (Arg "arg" (Maybe String) -> Id)
  , userName       :: Magic ts Name
  , userBestFriend :: Magic ts (Arg "arg" (Maybe String) -> User ts)
  , userFriends    :: Magic ts [User ts]
  , userAccount    :: Magic ts (Account ts)
  } deriving (Generic)

deriving instance AllHave Show (User ts) => Show (User ts)
deriving instance AllHave Eq (User ts)   => Eq (User ts)

data Account ts = Account
  { accountBalance :: Magic ts (Arg "num" (Maybe Int) -> Int)
  } deriving (Generic)

deriving instance AllHave Show (Account ts) => Show (Account ts)
deriving instance AllHave Eq (Account ts)   => Eq (Account ts)


------------------------------------------------------------------------------
-- | This module just instantiates a bunch of our generics at concrete types to
-- make sure we didn't miss any cases.
spec :: Spec
spec = it "should compile when we instantiate our generics to concrete types" $
  True `shouldBe` True


------------------------------------------------------------------------------


allTypesUser :: [Doc]
allTypesUser = allTypes @User

allTypesAccount :: [Doc]
allTypesAccount = allTypes @Account

emptyQueryUser :: User 'Query
emptyQueryUser = emptyQuery

emptyQueryAccount :: Account 'Query
emptyQueryAccount = emptyQuery

hydrateUser :: User 'Data -> User 'Query -> User 'Response
hydrateUser = hydrate

hydrateAccount :: Account 'Data -> Account 'Query -> Account 'Response
hydrateAccount = hydrate

pprQueryUser :: User 'Query -> Doc
pprQueryUser = pprQuery

pprQueryAccount :: Account 'Query -> Doc
pprQueryAccount = pprQuery

pprSchemaUser :: User 'Schema -> Doc
pprSchemaUser = pprSchema

pprSchemaAccount :: Account 'Schema -> Doc
pprSchemaAccount = pprSchema

queryParserUser :: ReaderT Vars Parser (User 'Query)
queryParserUser = queryParser

queryParserAccount :: ReaderT Vars Parser (Account 'Query)
queryParserAccount = queryParser

resolveUser :: User 'Resolver -> User 'Query -> IO (User 'Response)
resolveUser = resolve

resolveAccount :: Account 'Resolver -> Account 'Query -> IO (Account 'Response)
resolveAccount = resolve

schemaUser :: User 'Schema
schemaUser = schema

schemaAccount :: Account 'Schema
schemaAccount = schema

