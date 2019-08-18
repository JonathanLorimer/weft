module GenericsSpec where

import Control.Monad.Reader
import Test.Hspec hiding (Arg)
import TestData
import Text.PrettyPrint.HughesPJ (Doc)
import Weft.Generics.Hydrate
import Weft.Generics.PprQuery
import Weft.Generics.PprSchema
import Weft.Generics.QueryParser
import Weft.Generics.Resolve
import Weft.Generics.Schema
import Weft.Types
import Weft.Internal.Types



------------------------------------------------------------------------------
-- | This module just instantiates a bunch of our generics at concrete types to
-- make sure we didn't miss any cases.
spec :: Spec
spec = it "should compile when we instantiate our generics to concrete types" $
  True `shouldBe` True


------------------------------------------------------------------------------


hydrateUser :: User -> JHKD User 'Query -> JHKD User 'Response
hydrateUser = hydrate

hydrateAccount :: Account -> JHKD Account 'Query -> JHKD Account 'Response
hydrateAccount = hydrate

pprQueryUser :: JHKD User 'Query -> Doc
pprQueryUser = pprQuery

pprQueryAccount :: JHKD Account 'Query -> Doc
pprQueryAccount = pprQuery

pprSchemaUser :: JHKD User 'Schema -> Doc
pprSchemaUser = pprSchema

pprSchemaAccount :: JHKD Account 'Schema -> Doc
pprSchemaAccount = pprSchema

queryParserUser :: ReaderT Vars Parser (JHKD User 'Query)
queryParserUser = queryParser

queryParserAccount :: ReaderT Vars Parser (JHKD Account 'Query)
queryParserAccount = queryParser

resolveUser :: JHKD User 'Resolver -> JHKD User 'Query -> IO (JHKD User 'Response)
resolveUser = resolve

resolveAccount :: JHKD Account 'Resolver -> JHKD Account 'Query -> IO (JHKD Account 'Response)
resolveAccount = resolve

schemaUser :: JHKD User 'Schema
schemaUser = schema

schemaAccount :: JHKD Account 'Schema
schemaAccount = schema

