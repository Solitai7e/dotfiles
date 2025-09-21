module RawCommandReader (RawCommandReader(..)) where
import Xmobar (Exec(alias, start))
import System.IO (hGetLine)
import System.Process (getProcessExitCode, proc, withCreateProcess, CreateProcess(std_err, std_in, std_out), StdStream(Inherit, NoStream, CreatePipe))
import Control.Monad (MonadPlus(mzero), forever, when)
import Data.Maybe (isJust)

data RawCommandReader = RawCommandReader String [String] String
  deriving (Read, Show)

instance Exec RawCommandReader where
  alias (RawCommandReader _ _ a)    = a
  start (RawCommandReader arg0 args _) cb =
    withCreateProcess (proc arg0 args)
                      {std_in = NoStream,
                       std_out = CreatePipe,
                       std_err = Inherit} $
      \_ (Just hstdout) _ ph -> forever $ do
        hGetLine hstdout >>= cb
        ec <- getProcessExitCode ph
        when (isJust ec) (cb "EXITED" >> mzero)
