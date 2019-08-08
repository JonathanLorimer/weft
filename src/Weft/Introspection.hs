module Weft.Introspection where

data __Schema =
  __Schema { types            :: [__Type]
           , queryType        :: __Type
           , mutationType     :: Maybe __Type
           , subscriptionType :: Maybe __Type
           , directives       :: [__Directive]
           }

data __Type =
  __Type { kind          :: __TypeKind
         , name          :: Maybe String
         , description   :: Maybe String
         -- OBJECT and INTERFACE only
         , fields        :: Maybe (Arg "includeDeprecated" (Maybe Bool) -> [__Field])
         -- OBJECT only
         , interfaces    :: Maybe [__Field]
         -- INTERFACE and UNION only
         , possibleTypes :: Maybe [__Field]
         --  ENUM only
         , enumValues    :: Maybe (Arg "includeDeprecated" (Maybe Bool) -> [__EnumValue])
         -- INPUT_OBJECT only
         , inputFields   :: Maybe [__InputValue]
         -- NON_NULL and LIST only
         , ofType        :: Maybe __Type
         }

data __Field =
  __Field { name              :: String
          , description       :: Maybe String
          , args              :: [__InputValue]
          , _type             :: __Type
          , isDeprecated      :: Boolean
          , deprecationReason :: String
          }

data __InputValue =
  __InputValue { name         :: String
               , description  :: Maybe String
               , _type        :: __Type
               , defaultValue :: Maybe String
               }

data __EnumValue =
  __EnumValue { name              :: String
              , description       :: Maybe String
              , isDeprecated      :: Boolean
              , deprecationReason :: Maybe String
              }

data __TypeKind = TKSCALAR
                | TKOBJECT
                | TKINTERFACE
                | TKUNION
                | TKENUM
                | TKINPUT_OBJECT
                | TKLIST
                | TKNON_NULL

data __Directive =
  __Directive { name        :: String
              , description :: Maybe String
              , locations   :: [__DirectiveLocation]
              , args        :: [__InputValue]
              }

data __DirectiveLocation =
                         | DLQUERY
                         | DLMUTATION
                         | DLSUBSCRIPTION
                         | DLFIELD
                         | DLFRAGMENT_DEFINITION
                         | DLFRAGMENT_SPREAD
                         | DLINLINE_FRAGMENT
                         | DLSCHEMA
                         | DLSCALAR
                         | DLOBJECT
                         | DLFIELD_DEFINITION
                         | DLARGUMENT_DEFINITION
                         | DLINTERFACE
                         | DLUNION
                         | DLENUM
                         | DLENUM_VALUE
                         | DLINPUT_OBJECT
                         | DLINPUT_FIELD_DEFINITION