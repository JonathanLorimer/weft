module GenericsSpec where

import Control.Monad.Reader
import Test.Hspec hiding (Arg)
import TestData
import Text.PrettyPrint.HughesPJ (Doc)
import Weft.Generics.AllTypes
import Weft.Generics.Hydrate
import Weft.Generics.PprQuery
import Weft.Generics.PprSchema
import Weft.Generics.QueryParser
import Weft.Generics.Resolve
import Weft.Generics.Schema
import Weft.Types



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

