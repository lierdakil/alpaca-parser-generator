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
import Control.Monad.Identity
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

type MyMonadT m a = ExceptT [Text] (WriterT [Text] m) a
type MyMonad a = MyMonadT Identity a

runInIO :: MyMonadT IO () -> IO ()
runInIO = (>>= output) . runWriterT . runExceptT
  where output (eitherErrorResult, warning) = do
          mapM_ T.putStrLn warning
          either (T.putStrLn . T.unlines) return eitherErrorResult
