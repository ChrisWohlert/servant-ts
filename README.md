# servant-ts

[![CircleCI](https://circleci.com/gh/smaccoun/servant-ts.svg?style=svg)](https://circleci.com/gh/smaccoun/servant-ts)

# Example

Consider the following common User API

```haskell

type UserAPI = "user" :> Get '[JSON] [User]
              :<|> "user" :> Capture "userId" Int :> Get '[JSON] User

data User = User
    {name    :: Text
    ,age     :: Int
    ,isAdmin :: Bool
    ,hasMI   :: Maybe Text
    } deriving (Generic, TypescriptType)

```

Given a flavor configuration you can auto generate the following type and function declaration files for this API.

```Typescript
// Declarations

interface User { 
  name : string
  age : number
  isAdmin : boolean
  hasMI : Option<string>
}
```

```Typescript
// Function Declarations

function getUser(): Promise<User> {
  return fetch(withBaseUrl(`user`))
}

function getUserByUserId(userId : number): Promise<Array<User>> {
  return fetch(withBaseUrl(`user/${userId}`))
}
```


