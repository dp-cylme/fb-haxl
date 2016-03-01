module Facebook.Haxl (
   initGlobalState
   -- * API
  , getPage
  , getPagePosts
  , getPostLikes
  , getPostComments
  , getSharedPosts
  , getNextPager

   -- * Facebook types
  , Id(..)
  , Page(..)
  , User(..)
  ) where

import Data.Aeson (FromJSON)
import Data.Typeable (Typeable)
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


-- | Fetch post likes
getPostLikes :: Id -> [Argument] -> AccessToken anyKind -> GenHaxl u (Pager User)
getPostLikes pid args tok = dataFetch (GetPostLikes pid args tok)


-- | Fetch next batch of b
getNextPager :: (Show b, FromJSON b, Typeable b) => Pager b -> GenHaxl u (Maybe (Pager b))
getNextPager p = dataFetch (GetNextPager p)


-- | Fetch post comments
getPostComments :: Id -> [Argument] -> AccessToken anyKind -> GenHaxl u (Pager Comment)
getPostComments pid args tok = dataFetch (GetPostComments pid args tok)


-- | Fecth post reposts
getSharedPosts :: Id -> [Argument] -> AccessToken anyKind -> GenHaxl u (Pager Post)
getSharedPosts pid args tok = dataFetch (GetSharedPosts pid args tok)


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
