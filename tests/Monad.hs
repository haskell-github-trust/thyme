import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalStateT)
import Control.Monad.Thyme (currentTime)

main :: IO ()
main = do
  currentTime >>= print
  -- Test that generic MonadTrans instance works.
  runReaderT currentTime 'x' >>= print
  evalStateT (runReaderT currentTime 'x') 'y' >>= print
