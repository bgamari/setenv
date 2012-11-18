{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module System.SetEnv (
  setEnv
, unsetEnv
) where

import Foreign.C
import GHC.IO.Exception (IOErrorType (InvalidArgument))
import Control.Exception (throwIO)
import System.IO.Error (mkIOError)

#ifdef mingw32_HOST_OS
import GHC.Windows
import Foreign.Safe
import Control.Monad
#else
import qualified System.Posix.Env as Posix
import GHC.IO.Encoding (getFileSystemEncoding)
import qualified GHC.Foreign as GHC
#endif

#ifdef mingw32_HOST_OS
# if defined(i386_HOST_ARCH)
#  define WINDOWS_CCONV stdcall
# elif defined(x86_64_HOST_ARCH)
#  define WINDOWS_CCONV ccall
# else
#  error Unknown mingw32 arch
# endif

foreign import WINDOWS_CCONV unsafe "windows.h GetLastError"
  c_GetLastError:: IO DWORD

eRROR_ENVVAR_NOT_FOUND :: DWORD
eRROR_ENVVAR_NOT_FOUND = 203

#endif

-- | @setEnv name value@ sets the specified environment variable to @value@.
--
-- On Windows setting an environment variable to the /empty string/ removes
-- that environment variable from the environment.  For the sake of
-- compatibility we adopt that behavior.  In particular
--
-- @
-- setEnv name \"\"
-- @
--
-- has the same effect as
--
-- @
-- `unsetEnv` name
-- @
--
-- If you don't care about Windows support and want to set an environment
-- variable to the empty string use @System.Posix.Env.setEnv@ from the @unix@
-- package instead.
--
-- Throws `Control.Exception.IOException` if @name@ is the empty string or
-- contains an equals sign.
setEnv :: String -> String -> IO ()
setEnv key_ value_
  | null key       = throwIO (mkIOError InvalidArgument "setEnv" Nothing Nothing)
  | '=' `elem` key = throwIO (mkIOError InvalidArgument "setEnv" Nothing Nothing)
  | null value     = unsetEnv key
  | otherwise      = setEnv_ key value
  where
    key   = takeWhile (/= '\NUL') key_
    value = takeWhile (/= '\NUL') value_

setEnv_ :: String -> String -> IO ()
#ifdef mingw32_HOST_OS
setEnv_ key value = withCWString key $ \k -> withCWString value $ \v -> do
  success <- c_SetEnvironmentVariable k v
  unless success (throwGetLastError "setEnv")

foreign import WINDOWS_CCONV unsafe "windows.h SetEnvironmentVariableW"
  c_SetEnvironmentVariable :: LPTSTR -> LPTSTR -> IO Bool
#else

-- NOTE: The 'setenv()' function is not available on all systems, hence we use
-- 'putenv()'.  This leaks memory, but so do common implementations of
-- 'setenv()' (AFAIK).
setEnv_ k v = putEnv (k ++ "=" ++ v)

putEnv :: String -> IO ()
putEnv keyvalue = do
  s <- getFileSystemEncoding >>= (`GHC.newCString` keyvalue)
  -- IMPORTANT: Do not free `s` after calling putenv!
  --
  -- According to SUSv2, the string passed to putenv becomes part of the
  -- enviroment.
  throwErrnoIfMinus1_ "putenv" (c_putenv s)

foreign import ccall unsafe "putenv" c_putenv :: CString -> IO CInt
#endif

-- | @unSet name@ removes the specified environment variable from the
-- environment of the current process.
--
-- Throws `Control.Exception.IOException` if @name@ is the empty string or
-- contains an equals sign.
unsetEnv :: String -> IO ()
#ifdef mingw32_HOST_OS
unsetEnv key = withCWString key $ \k -> do
  success <- c_SetEnvironmentVariable k nullPtr
  unless success $ do
    -- We consider unsetting an environment variable that does not exist not as
    -- an error, hence we ignore eRROR_ENVVAR_NOT_FOUND.
    err <- c_GetLastError
    unless (err == eRROR_ENVVAR_NOT_FOUND) $ do
      throwGetLastError "unsetEnv"
#else
unsetEnv = Posix.unsetEnv
#endif
