module MonadTypes (
    module MonadTypes
  , throwError
  , catchError
  , lift
  , liftEither
  , tell
  , censor
  ) where

import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.Identity

type MyMonadT m a = ExceptT [String] (WriterT [String] m) a
type MyMonad a = MyMonadT Identity a

runInIO :: MyMonadT IO () -> IO ()
runInIO = (>>= output) . runWriterT . runExceptT
  where output (eitherErrorResult, warning) = do
          mapM_ putStrLn warning
          either (putStrLn . unlines) return eitherErrorResult
