{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoMonoLocalBinds           #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}

module Main where

import Network.Wai.Handler.Warp
import Weft.Server
import Weft.Types
import Weft.Generics.Resolve
import Weft.Generics.Hydrate

data GqlQuery ts = GqlQuery
    { getUser :: Magic ts (Arg "id" ID -> User ts)
    , getAllUsers :: Magic ts [User ts]
    } deriving (Generic)

data User ts = User
  { userId         :: Magic ts (Arg "arg" (Maybe Int) -> ID)
  , userName       :: Magic ts String
  , userBestFriend :: Magic ts (Arg "arg" (Maybe Int) -> User ts)
  , userFriends    :: Magic ts [User ts]
  } deriving (Generic)

deriving instance AllHave Show (User ts)     => Show (User ts)
deriving instance AllHave Eq (User ts)       => Eq (User ts)

data Account ts = Account
  { accountBalance :: Magic ts (Arg "num" (Maybe Int) -> Int)
  } deriving (Generic)

deriving instance AllHave Show (Account ts) => Show (Account ts)
deriving instance AllHave Eq (Account ts)   => Eq (Account ts)

jonathan :: User 'Data
jonathan = User { userId = (ID "1"), userName = ( "Jonathan"), userBestFriend = sandy, userFriends = [] }

sandy :: User 'Data
sandy = User { userId = (ID "2"), userName = ( "Sandy"), userBestFriend = jonathan, userFriends = []}

getUserResolver :: (Arg "id" ID) -> User 'Query -> IO (User 'Response)
getUserResolver a q
    | (getArg a) == (ID "1") = pure $ hydrate jonathan q
    | (getArg a) == (ID "2") = pure $ hydrate sandy q
    | otherwise = pure $ hydrate jonathan q

getAllUsersResolver :: User 'Query -> IO [User 'Response]
getAllUsersResolver q = pure $ hydrateF [sandy, jonathan] q

queryResolver :: GqlQuery 'Resolver
queryResolver = GqlQuery
            { getUser = getUserResolver
            , getAllUsers = getAllUsersResolver
            }

noneResolver :: None 'Resolver
noneResolver = None (pure ())

gqlResolver :: Gql GqlQuery None s 'Resolver
gqlResolver = Gql (resolve queryResolver) (resolve noneResolver)


deriving instance Show (GqlQuery 'Response)
deriving instance Show (GqlQuery 'Query)

main :: IO ()
main = do
    let port = id @Int 3000
    let settings = [setPort port]
    Prelude.putStrLn $ "Listening on port " ++ show port
    server settings gqlResolver

