{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MonadTypes (
    module MonadTypes
  , throwError
  , catchError
  , lift
  , liftEither
  , tell
  , censor
  , listen
  ) where

import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Identity
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Grammar.Parse

newtype MyMonadT m a = MyMonadT { unMyMonadT :: StateT [(Symbol,Type)] (ExceptT [Text] (WriterT [Text] m)) a }
  deriving  (Functor, Applicative, Monad, MonadError [Text], MonadWriter [Text], MonadState [(Symbol,Type)])
type MyMonad a = MyMonadT Identity a

instance MonadTrans MyMonadT where
  lift = MyMonadT . lift . lift . lift

runInIO :: MyMonadT IO () -> IO ()
runInIO = (>>= output) . runWriterT . runExceptT . flip evalStateT [] . unMyMonadT
  where output (eitherErrorResult, warning) = do
          mapM_ T.putStrLn warning
          either (T.putStrLn . T.unlines) return eitherErrorResult

runMyMonad :: MyMonad a -> (Either [Text] a, [Text])
runMyMonad = runIdentity . runWriterT . runExceptT . flip evalStateT [] . unMyMonadT
