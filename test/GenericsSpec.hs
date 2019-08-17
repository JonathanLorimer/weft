module GenericsSpec where

import Control.Monad.Reader
import Test.Hspec hiding (Arg)
import BizzaroData
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
hydrateUser = magicHydrate

hydrateAccount :: Account -> JHKD Account 'Query -> JHKD Account 'Response
hydrateAccount = magicHydrate

pprQueryUser :: JHKD User 'Query -> Doc
pprQueryUser = magicPprQuery

pprQueryAccount :: JHKD Account 'Query -> Doc
pprQueryAccount = magicPprQuery

pprSchemaUser :: JHKD User 'Schema -> Doc
pprSchemaUser = magicPprSchema

pprSchemaAccount :: JHKD Account 'Schema -> Doc
pprSchemaAccount = magicPprSchema

queryParserUser :: ReaderT Vars Parser (JHKD User 'Query)
queryParserUser = magicQueryParser

queryParserAccount :: ReaderT Vars Parser (JHKD Account 'Query)
queryParserAccount = magicQueryParser

-- resolveUser :: JHKD User 'Resolver -> JHKD User 'Query -> IO (JHKD User 'Response)
-- resolveUser = resolve

-- resolveAccount :: JHKD Account 'Resolver -> JHKD Account 'Query -> IO (JHKD Account 'Response)
-- resolveAccount = resolve

schemaUser :: JHKD User 'Schema
schemaUser = magicSchema

schemaAccount :: JHKD Account 'Schema
schemaAccount = magicSchema

