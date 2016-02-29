{-# LANGUAGE OverloadedStrings, StandaloneDeriving, RecordWildCards,
     GADTs, TypeFamilies, MultiParamTypeClasses, DeriveDataTypeable,
     FlexibleInstances, RankNTypes #-}
module Facebook.Haxl.DataSource where

import Data.Typeable (Typeable)
import Data.Hashable (Hashable(..))
import Facebook
       (Id(..), Page(..), Credentials, AccessToken(..), FacebookT,
        getPage, runFacebookT, Argument, Post, getPagePosts, Pager,
        fetchNextPage, getLikes, User, Comment, getComments, getSharedPosts)
import Network.HTTP.Conduit (Manager)
import Control.Concurrent.QSem (newQSem, QSem, waitQSem, signalQSem)
import Control.Concurrent.Async (wait, Async(..), async)
import Control.Exception (try, bracket_, SomeException)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)

import Haxl.Core


data FacebookReq a where
        GetPage ::
          Id -> [Argument] -> AccessToken anyKind -> FacebookReq Page
        GetPagePosts ::
          Id -> [Argument] -> AccessToken anyKind -> FacebookReq (Pager Post)
        GetNextPagePosts :: Pager Post -> FacebookReq (Maybe (Pager Post))
        GetPostLikes ::
          Id -> [Argument] -> AccessToken anyKind -> FacebookReq (Pager User)
        GetNextPostLikes :: Pager User -> FacebookReq (Maybe (Pager User))
        GetPostComments ::
          Id ->
            [Argument] -> AccessToken anyKind -> FacebookReq (Pager Comment)
        GetNextPostComments ::
          Pager Comment -> FacebookReq (Maybe (Pager Comment))
        GetSharedPosts ::
          Id -> [Argument] -> AccessToken anyKind -> FacebookReq (Pager Post)
        GetNextSharedPosts ::
          Pager Post -> FacebookReq (Maybe (Pager Post))
    deriving Typeable

instance Eq (FacebookReq a) where
    (==) (GetPage pid args _) (GetPage pid' args' _) =
        (pid == pid') && (args == args')
    (==) (GetPagePosts pid args _) (GetPagePosts pid' args' _) =
        (pid == pid') && (args == args')
    (==) (GetPostLikes pid args _) (GetPostLikes pid' args' _) =
        (pid == pid') && (args == args')
    (==) (GetPostComments pid args _) (GetPostComments pid' args' _) =
        (pid == pid') && (args == args')
    (==) (GetSharedPosts pid args _) (GetSharedPosts pid' args' _) =
        (pid == pid') && (args == args')
    (==) _ _ = False


deriving instance Show (FacebookReq a)

instance Show1 FacebookReq where show1 = show

instance Hashable (FacebookReq a) where
    hashWithSalt s (GetPage (Id pid) _ _) = hashWithSalt s (0 :: Int, pid)
    hashWithSalt s (GetPagePosts (Id pid) _ _) = hashWithSalt s (1 :: Int, pid)
    hashWithSalt s (GetPostLikes (Id pid) _ _) = hashWithSalt s (2 :: Int, pid)
    hashWithSalt s (GetPostComments (Id pid) _ _) =
        hashWithSalt s (3 :: Int, pid)
    hashWithSalt s (GetSharedPosts (Id pid) _ _) =
        hashWithSalt s (4 :: Int, pid)


instance StateKey FacebookReq where
    data State FacebookReq = FacebookState{ credentials :: Credentials
                                          , manager :: Manager
                                          , numThreads :: Int}


instance DataSourceName FacebookReq where
  dataSourceName _ = "Facebook"

instance DataSource u FacebookReq where
    fetch = facebookFetch


facebookFetch
  :: State FacebookReq
  -> Flags
  -> u
  -> [BlockedFetch FacebookReq]
  -> PerformFetch

facebookFetch FacebookState{..} _flags _user bfs =
  AsyncFetch $ \inner -> do
    sem <- newQSem numThreads
    asyncs <- mapM (fetchAsync credentials manager sem) bfs
    inner
    mapM_ wait asyncs

fetchAsync :: Credentials
           -> Manager
           -> QSem
           -> BlockedFetch FacebookReq
           -> IO (Async ())
fetchAsync creds manager sem (BlockedFetch req rvar) =
  async $ bracket_ (waitQSem sem) (signalQSem sem) $ do
    e <- Control.Exception.try $
           runResourceT $ runFacebookT creds manager $ fetchReq req
    case e of
      Left ex -> putFailure rvar (ex :: SomeException)
      Right a -> putSuccess rvar a

fetchReq :: FacebookReq a -> FacebookT anyAuth (ResourceT IO) a
fetchReq (GetPage pid args tok) = getPage pid args (Just tok)
fetchReq (GetPagePosts pid args tok) = getPagePosts pid args (Just tok)
fetchReq (GetNextPagePosts pager) = fetchNextPage pager
fetchReq (GetPostLikes pid args tok) = getLikes pid args tok
fetchReq (GetNextPostLikes pager) = fetchNextPage pager
fetchReq (GetPostComments pid args tok) = getComments pid args tok
fetchReq (GetNextPostComments pager) = fetchNextPage pager
fetchReq (GetSharedPosts pid args tok) = getSharedPosts pid args tok
fetchReq (GetNextSharedPosts pager) = fetchNextPage pager

