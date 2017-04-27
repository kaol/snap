{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE TemplateHaskell     #-}

module Snap.Snaplet.Internal.Tests
  ( tests, initTest ) where

------------------------------------------------------------------------------
import           Control.Exception                   (finally)
import           Control.Lens                        (makeLenses)
import           Control.Monad.Trans                 (MonadIO, liftIO)
import           Control.Monad.Trans.Except          (ExceptT(..), runExceptT)
import           Data.ByteString                     (ByteString)
import qualified Data.ByteString.Char8               as B
import           Data.IORef                          (newIORef, modifyIORef,
                                                      readIORef)
import qualified Data.Map                            as Map
import           Data.Text                           (Text)
import           Prelude                             hiding (catch, (.))
import           System.Directory                    (getCurrentDirectory)
import           Test.Framework                      (Test, testGroup)
import           Test.Framework.Providers.HUnit      (testCase)
import           Test.Framework.Providers.SmallCheck (testProperty)
import           Test.HUnit                          hiding (Test, path)
import           Test.SmallCheck                     ((==>))
------------------------------------------------------------------------------
import           Snap.Core                           (rspStatus)
import           Snap.Snaplet                        (with)
import           Snap.Snaplet.Heist                  (heistServe, gHeistServe)
import           Snap.Snaplet.Internal.Initializer
import           Snap.Snaplet.Internal.Types
import           Snap.Snaplet.Test                   (getSnaplet, runHandler',
                                                      closeSnaplet)
import qualified Snap.Snaplet.Test.Common.App        as C
import qualified Snap.Test                           as ST


                       ---------------------------------
                       -- TODO: this module is a mess --
                       ---------------------------------


------------------------------------------------------------------------------
data Foo = Foo Int

data Bar = Bar Int


data App = App
    { _foo :: Snaplet Foo
    , _bar :: Snaplet Bar
    }

makeLenses ''App

--showConfig :: SnapletConfig -> IO ()
--showConfig c = do
--    putStrLn "SnapletConfig:"
--    print $ _scAncestry c
--    print $ _scFilePath c
--    print $ _scId c
--    print $ _scDescription c
--    print $ _scRouteContext c
--    putStrLn ""


------------------------------------------------------------------------------
assertGet :: (MonadIO m, Show a, Eq a) => String -> m a -> a -> m ()
assertGet name getter val = do
    v <- getter
    liftIO $ assertEqual name val v


------------------------------------------------------------------------------
configAssertions :: (MonadSnaplet m, MonadIO (m b v))
                 => [Char]
                 -> ([Text], FilePath, Maybe Text, Text, ByteString)
                 -> m b v ()
configAssertions prefix (a,f,n,d,r) = do
    assertGet (prefix ++ "ancestry"      ) getSnapletAncestry    a
    assertGet (prefix ++ "file path"     ) getSnapletFilePath    f
    assertGet (prefix ++ "name"          ) getSnapletName        n
    assertGet (prefix ++ "description"   ) getSnapletDescription d
    assertGet (prefix ++ "route context" ) getSnapletRootURL     r


------------------------------------------------------------------------------
appInit :: SnapletInit App App
appInit = makeSnaplet "app" "Test application" Nothing $ do
    cwd <- liftIO getCurrentDirectory

    configAssertions "root "
        ([], cwd, Just "app", "Test application", "")

    assertGet "environment" getEnvironment "devel"

    f <- nestSnaplet "foo" foo $ fooInit
    b <- nestSnaplet "bar" bar $ barInit
    return $ App f b


------------------------------------------------------------------------------
fooInit :: SnapletInit b Foo
fooInit = makeSnaplet "foo" "Foo Snaplet" Nothing $ do
    cwd <- liftIO getCurrentDirectory
    let dir = cwd ++ "/snaplets/foo"

    configAssertions "foo "
        (["app"], dir, Just "foo", "Foo Snaplet", "foo")
    return $ Foo 42


------------------------------------------------------------------------------
barInit :: SnapletInit b Bar
barInit = makeSnaplet "bar" "Bar Snaplet" Nothing $ do
    cwd <- liftIO getCurrentDirectory
    let dir = cwd ++ "/snaplets/bar"
    configAssertions "bar "
        (["app"], dir, Just "bar", "Bar Snaplet", "bar")
    return $ Bar 2


------------------------------------------------------------------------------
initTest :: IO ()
initTest = do
    (out,_,_) <- runSnaplet Nothing appInit

    -- note from gdc: wtf?
    if out == "aoeu"
      then putStrLn "Something really strange"
      else return ()


------------------------------------------------------------------------------
bracketHandlerTest :: IO ()
bracketHandlerTest = do
    refAcq <- newIORef 0
    refRel <- newIORef 0
    res <- runExceptT $ do
        (s,i) <- ExceptT $ getSnaplet Nothing C.appInit
        let hdl = with C.heist $ bracketHandler
              (modifyIORef refAcq (+1))
              (const $ modifyIORef refRel (+1))
              (const gHeistServe)
        ExceptT $ flip finally (closeSnaplet i) $ runExceptT $ do
            good <- runHandler' s i (ST.get "index" Map.empty) hdl
            bad <- runHandler' s i (ST.get "404" Map.empty) hdl
            return (good, bad)
    acquires <- readIORef refAcq
    releases <- readIORef refRel
    assertEqual "bracketHandler acquire and release won't be called on short circuit"
      (Right (Right 200, Right 404), 1, 1)
      (fmap (\(g,b) -> (fmap rspStatus g, fmap rspStatus b)) res, acquires, releases)


------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Snap.Snaplet.Internal"
    [ testCase "initializer tests" initTest
    , testCase "bracketHandler test" bracketHandlerTest
    , testProperty "buildPath generates no double slashes" doubleSlashes
    ]

--doubleSlashes :: Monad m => [String] -> Property m
doubleSlashes arrStr = noSlashes ==> not (B.isInfixOf "//" $ buildPath arr)
  where
    arr = map B.pack arrStr
    noSlashes = not $ or $ map (B.elem '/') arr


