module TestData where

import GHC.Generics
import Data.Text
import SchemaGenerator

newtype Id = Id String deriving (Generic, Show)
newtype Name = Name String deriving (Generic, Show)

data User' ts = User
  { userId         :: Magic ts '[ '("the_arg", Maybe Int) ] Id
  , userName       :: Magic ts '[ '("other_arg", String) ] Name
  , userBestFriend :: Magic ts '[] (User' ts)
  , userFriends    :: Magic ts '[] [User' ts]
  } deriving (Generic)
deriving instance Show (User' 'Schema)
deriving instance Show (User' 'Response)
deriving instance Show (User' 'Query)

type User = User' 'Data
type UserQuery = User' 'Query

userQ :: User' 'Query
userQ = User Nothing Nothing Nothing Nothing

sampleBody :: Text
sampleBody = "\n# Welcome to GraphiQL\n#\n# GraphiQL is an in-browser tool for writing, validating, and\n# testing GraphQL queries.\n#\n# Type queries into this side of the screen, and you will see intelligent\n# typeaheads aware of the current GraphQL type schema and live syntax and\n# validation errors highlighted within the text.\n#\n# GraphQL queries typically start with a \"{\" character. Lines that starts\n# with a # are ignored.\n#\n# An example GraphQL query might look like:\n#\n#     {\n#       field(arg: \"value\") {\n#         subField\n#       }\n#     }\n#\n# Keyboard shortcuts:\n#\n#       Run Query:  Ctrl-Enter (or press the play button above)\n#\n#   Auto Complete:  Ctrl-Space (or just start typing)\n#\n# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #\n# Default endpoint is an instance of https://www.graph.cool/\n# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #\n\nquery {\n  allUsers {\n    id\n    name\n  }\n}\n"
