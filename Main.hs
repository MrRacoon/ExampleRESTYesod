{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}


import Control.Monad.Logger (runNoLoggingT)
import Data.Text            (Text)
import Data.Time
import Database.Persist.Sqlite
import Yesod
import Text.Hamlet
import Text.Blaze


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
  name Text
  age  Int
|]


instance ToJSON Person where
  toJSON Person {..} = object
    [ "name" .= personName
    , "age"  .= personAge
    ]


instance ToJSON (Entity Person) where
  toJSON (Entity pid person) = object
    [ "name" .= personName person
    , "age"  .= personAge person
    ]


data App = App ConnectionPool


mkYesod "App" [parseRoutes|
/                  HomeR   GET
/people/#Text/#Int PeopleR PUT GET
|]


instance Yesod App


instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage


instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB db = do
    App pool <- getYesod
    runSqlPool db pool


getHomeR :: Handler Html
getHomeR =  do
    lst <- runDB $ selectList [PersonAge >. 0] []
    return
        [shamlet|
          <h2> Yo
          <ul>
            $forall Entity prsnid prsn <- lst
              <p> #{personName prsn} and #{personAge prsn}
        |]


getPeopleR :: Text -> Int -> Handler TypedContent
getPeopleR name age = do
    lst <- runDB $ selectList [PersonAge ==. age] []
    selectRep $ do
      provideRep $ return
        [shamlet|
          <h2> Filtered
          <ul>
            $forall Entity prsnid prsn <- lst
              <p> #{personName prsn} and #{personAge prsn}
        |]
      provideRep $ do
        return $ array lst


putPeopleR :: Text -> Int -> Handler ()
putPeopleR name age = do
    runDB $ insert $ Person name age
    redirect HomeR


main :: IO ()
main = runNoLoggingT $ withSqlitePool "links.db3" 10 $ \pool -> liftIO $ do
  runSqlPersistMPool (runMigration migrateAll) pool
  warp 3000 $ App pool
