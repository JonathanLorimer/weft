module Weft.Introspection where

import Weft.Types

data Schema =
  Schema { schemaTypes            :: [Type]
         , schemaQueryType        :: Type
         , schemaMutationType     :: Maybe Type
         , schemaSubscriptionType :: Maybe Type
         , schemaDirectives       :: [Directive]
         }

data Type =
  Type { typeKind          :: TypeKind
       , typeName          :: Maybe String
       , typeDescription   :: Maybe String
       -- OBJECT and INTERFACE only
       , typeFields        :: Maybe (Arg "includeDeprecated" (Maybe Bool) -> [Field])
       -- OBJECT only
       , typeInterfaces    :: Maybe [Field]
       -- INTERFACE and UNION only
       , typePossibleTypes :: Maybe [Field]
       --  ENUM only
       , typeEnumValues    :: Maybe (Arg "includeDeprecated" (Maybe Bool) -> [EnumValue])
       -- INPUT_OBJECT only
       , typeInputFields   :: Maybe [InputValue]
       -- NON_NULL and LIST only
       , typeOfType        :: Maybe Type
       }

data Field =
  Field { fieldName              :: String
        , fieldDescription       :: Maybe String
        , fieldArgs              :: [InputValue]
        , fieldType              :: Type
        , fieldIsDeprecated      :: Bool
        , fieldDeprecationReason :: String
        }

data InputValue =
  InputValue { inputvalueName           :: String
             , inputvalueDescription  :: Maybe String
             , inputvalueType        :: Type
             , inputvalueDefaultValue :: Maybe String
             }

data EnumValue =
  EnumValue { enumvalueName              :: String
            , enumvalueDescription       :: Maybe String
            , enumvalueIsDeprecated      :: Bool
            , enumvalueDeprecationReason :: Maybe String
            }

data TypeKind = TypekindSCALAR
              | TypekindOBJECT
              | TypekindINTERFACE
              | TypekindUNION
              | TypekindENUM
              | TypekindINPUT_OBJECT
              | TypekindLIST
              | TypekindNON_NULL

data Directive =
  Directive { directiveName        :: String
            , directiveDescription :: Maybe String
            , directiveLocations   :: [DirectiveLocation]
            , directiveArgs        :: [InputValue]
            }

data DirectiveLocation = DirectivelocationQUERY
                       | DirectivelocationMUTATION
                       | DirectivelocationSUBSCRIPTION
                       | DirectivelocationFIELD
                       | DirectivelocationFRAGMENT_DEFINITION
                       | DirectivelocationFRAGMENT_SPREAD
                       | DirectivelocationINLINE_FRAGMENT
                       | DirectivelocationSCHEMA
                       | DirectivelocationSCALAR
                       | DirectivelocationOBJECT
                       | DirectivelocationFIELD_DEFINITION
                       | DirectivelocationARGUMENT_DEFINITION
                       | DirectivelocationINTERFACE
                       | DirectivelocationUNION
                       | DirectivelocationENUM
                       | DirectivelocationENUM_VALUE
                       | DirectivelocationINPUT_OBJECT
                       | DirectivelocationINPUT_FIELD_DEFINITION