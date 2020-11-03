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

type MyMonadT m a = StateT [(Symbol,Type)] (ExceptT [Text] (WriterT [Text] m)) a
type MyMonad a = MyMonadT Identity a

runInIO :: MyMonadT IO () -> IO ()
runInIO = (>>= output) . runWriterT . runExceptT . flip evalStateT []
  where output (eitherErrorResult, warning) = do
          mapM_ T.putStrLn warning
          either (T.putStrLn . T.unlines) return eitherErrorResult

runMyMonad :: MyMonad a -> (Either [Text] a, [Text])
runMyMonad = runIdentity . runWriterT . runExceptT . flip evalStateT []
