module Facebook.Haxl (
   initGlobalState
   -- * API
  , getPage
  , getPagePosts
  , getNextPagePosts
  , getPostLikes
  , getNextPostLikes
  , getPostComments
  , getNextPostComments
  , getSharedPosts
  , getNextSharedPosts

   -- * Facebook types
  , Id(..)
  , Page(..)
  , User(..)
  ) where

import Network.HTTP.Conduit (Manager)
import Facebook
       (Id(..), Argument, AccessToken(..), Pager(..), Post(..), Page(..),
        Credentials, User(..), Comment(..))
import Haxl.Core
import Facebook.Haxl.DataSource


-- | Fetch a Facebook page.
getPage :: Id
        -> [Argument]
        -> AccessToken anyKind
        -> GenHaxl u Page
getPage pid args tok = dataFetch (GetPage pid args tok)


-- | Fetch posts of Facebook page
getPagePosts :: Id
             -> [Argument]
             -> AccessToken anyKind
             -> GenHaxl u (Pager Post)
getPagePosts pid args tok = dataFetch (GetPagePosts pid args tok)


-- | Fetch next batch of posts of Facebook page
getNextPagePosts :: Pager Post -> GenHaxl u (Maybe (Pager Post))
getNextPagePosts p = dataFetch (GetNextPagePosts p)


-- | Fetch post likes
getPostLikes :: Id -> [Argument] -> AccessToken anyKind -> GenHaxl u (Pager User)
getPostLikes pid args tok = dataFetch (GetPostLikes pid args tok)


-- | Fetch next batch of likes of post
getNextPostLikes :: Pager User -> GenHaxl u (Maybe (Pager User))
getNextPostLikes p = dataFetch (GetNextPostLikes p)


-- | Fetch post comments
getPostComments :: Id -> [Argument] -> AccessToken anyKind -> GenHaxl u (Pager Comment)
getPostComments pid args tok = dataFetch (GetPostComments pid args tok)


-- | Fetch next batch of comments of post
getNextPostComments :: Pager Comment -> GenHaxl u (Maybe (Pager Comment))
getNextPostComments p = dataFetch (GetNextPostComments p)


-- | Fecth post reposts
getSharedPosts :: Id -> [Argument] -> AccessToken anyKind -> GenHaxl u (Pager Post)
getSharedPosts pid args tok = dataFetch (GetSharedPosts pid args tok)


-- | Fecth next batch of reposts of post
getNextSharedPosts :: Pager Post -> GenHaxl u (Maybe (Pager Post))
getNextSharedPosts p = dataFetch (GetNextSharedPosts p)


initGlobalState :: Int
                -> Credentials
                -> Manager
                -> IO (State FacebookReq)
initGlobalState threads creds mgr =
    return
        FacebookState
        { credentials = creds
        , manager = mgr
        , numThreads = threads
        }
