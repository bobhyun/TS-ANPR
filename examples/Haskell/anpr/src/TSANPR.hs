{-
- The MIT License (MIT)
- Copyright © 2022-2025 TS-Solution Corp.
-
- Permission is hereby granted, free of charge, to any person obtaining a copy
- of this software and associated documentation files (the "Software"), to deal
- in the Software without restriction, including without limitation the rights
- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
- copies of the Software, and to permit persons to whom the Software is
- furnished to do so, subject to all conditions.
-
- The above copyright notice and this permission notice shall be included in all
- copies or substantial portions of the Software.
-
- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
- SOFTWARE.
-}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module TSANPR where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

-- | TSANPR function pointers structure
data TSANPR = TSANPR
  { tsanprInitialize :: FunPtr (CString -> IO CString)
  , tsanprReadFile :: FunPtr (CString -> CString -> CString -> IO CString)
  , tsanprReadPixels :: FunPtr (Ptr CUChar -> CULong -> CULong -> CLong -> CString -> CString -> CString -> IO CString)
  }

-- | FunPtr to Haskell function converters
foreign import ccall "dynamic"
  mkAnprInitialize :: FunPtr (CString -> IO CString) -> (CString -> IO CString)
foreign import ccall "dynamic"
  mkAnprReadFile :: FunPtr (CString -> CString -> CString -> IO CString) -> (CString -> CString -> CString -> IO CString)
foreign import ccall "dynamic"
  mkAnprReadPixels :: FunPtr (Ptr CUChar -> CULong -> CULong -> CLong -> CString -> CString -> CString -> IO CString)
                   -> (Ptr CUChar -> CULong -> CULong -> CLong -> CString -> CString -> CString -> IO CString)

-- | Global reference to loaded library handle
{-# NOINLINE libHandle #-}
libHandle :: IORef (Maybe (Ptr ()))
libHandle = unsafePerformIO $ newIORef Nothing

-- | Global TSANPR instance
{-# NOINLINE tsanprInstance #-}
tsanprInstance :: IORef (Maybe TSANPR)
tsanprInstance = unsafePerformIO $ newIORef Nothing

-- | Foreign imports for dynamic library loading
#ifdef mingw32_HOST_OS
foreign import stdcall "windows.h LoadLibraryW"
  c_LoadLibrary :: CWString -> IO (Ptr ())
foreign import stdcall "windows.h FreeLibrary"
  c_FreeLibrary :: Ptr () -> IO Bool
foreign import stdcall "windows.h GetProcAddress"
  c_GetProcAddress :: Ptr () -> CString -> IO (FunPtr a)
#else
foreign import ccall "dlfcn.h dlopen"
  c_dlopen :: CString -> CInt -> IO (Ptr ())
foreign import ccall "dlfcn.h dlclose"
  c_dlclose :: Ptr () -> IO CInt
foreign import ccall "dlfcn.h dlsym"
  c_dlsym :: Ptr () -> CString -> IO (FunPtr a)
foreign import ccall "dlfcn.h dlerror"
  c_dlerror :: IO CString
#endif

-- | Constants
rtldLazy :: CInt
rtldLazy = 1

-- | Load TSANPR library
tsanprLoad :: String -> IO (Either String TSANPR)
tsanprLoad engineFileName = do
  tsanprUnload
  handle <-
#ifdef mingw32_HOST_OS
    withCWString engineFileName $ \cEngineFileName -> do
      h <- c_LoadLibrary cEngineFileName
      if h == nullPtr then return Nothing else return (Just h)
#else
    withCString engineFileName $ \cEngineFileName -> do
      h <- c_dlopen cEngineFileName rtldLazy
      if h == nullPtr then return Nothing else return (Just h)
#endif
  case handle of
    Nothing -> return $ Left "Cannot load module"
    Just h -> do
      writeIORef libHandle (Just h)
      -- Get function pointers
      initPtr   <- getFunctionPtr h "anpr_initialize"
      filePtr   <- getFunctionPtr h "anpr_read_file"
      pixelsPtr <- getFunctionPtr h "anpr_read_pixels"
      case (initPtr, filePtr, pixelsPtr) of
        (Just init', Just file', Just pixels') -> do
          let tsanpr = TSANPR init' file' pixels'
          writeIORef tsanprInstance (Just tsanpr)
          return $ Right tsanpr
        _ -> do
          tsanprUnload
          return $ Left "Failed to get function pointers"

-- | Get function pointer from library
getFunctionPtr :: Ptr () -> String -> IO (Maybe (FunPtr a))
getFunctionPtr handle funcName =
  withCString funcName $ \cFuncName -> do
#ifdef mingw32_HOST_OS
    ptr <- c_GetProcAddress handle cFuncName
#else
    ptr <- c_dlsym handle cFuncName
#endif
    if castFunPtrToPtr ptr == nullPtr
      then return Nothing
      else return (Just ptr)

-- | Unload TSANPR library
tsanprUnload :: IO ()
tsanprUnload = do
  handle <- readIORef libHandle
  case handle of
    Nothing -> return ()
    Just h -> do
#ifdef mingw32_HOST_OS
      _ <- c_FreeLibrary h
#else
      _ <- c_dlclose h
#endif
      writeIORef libHandle Nothing
      writeIORef tsanprInstance Nothing

-- | Initialize ANPR with parameters
anprInitialize :: String -> IO (Either String String)
anprInitialize initParams = do
  maybeTsanpr <- readIORef tsanprInstance
  case maybeTsanpr of
    Nothing -> return $ Left "TSANPR not loaded"
    Just tsanpr -> do
      result <- withCString initParams $ \cInitParams -> do
        cResult <- mkAnprInitialize (tsanprInitialize tsanpr) cInitParams
        if cResult == nullPtr
          then return Nothing
          else Just <$> peekCString cResult
      case result of
        Nothing -> return $ Right ""
        Just err -> return $ if null err then Right "" else Left err

-- | Read license plate from image file
anprReadFile :: String -> String -> String -> IO (Either String String)
anprReadFile imgFile outputFormat options = do
  maybeTsanpr <- readIORef tsanprInstance
  case maybeTsanpr of
    Nothing -> return $ Left "TSANPR not loaded"
    Just tsanpr -> do
      result <- withCString imgFile $ \cImgFile ->
        withCString outputFormat $ \cOutputFormat ->
          withCString options $ \cOptions -> do
            cResult <- mkAnprReadFile (tsanprReadFile tsanpr) cImgFile cOutputFormat cOptions
            if cResult == nullPtr
              then return Nothing
              else Just <$> peekCString cResult
      case result of
        Nothing -> return $ Left "Failed to read file"
        Just res -> return $ Right res

-- | Read license plate from pixel buffer
anprReadPixels :: Ptr CUChar -> CULong -> CULong -> CLong -> String -> String -> String -> IO (Either String String)
anprReadPixels pixels width height stride pixelFormat outputFormat options = do
  maybeTsanpr <- readIORef tsanprInstance
  case maybeTsanpr of
    Nothing -> return $ Left "TSANPR not loaded"
    Just tsanpr -> do
      result <- withCString pixelFormat $ \cPixelFormat ->
        withCString outputFormat $ \cOutputFormat ->
          withCString options $ \cOptions -> do
            cResult <- mkAnprReadPixels (tsanprReadPixels tsanpr) pixels width height stride cPixelFormat cOutputFormat cOptions
            if cResult == nullPtr
              then return Nothing
              else Just <$> peekCString cResult
      case result of
        Nothing -> return $ Left "Failed to read pixels"
        Just res -> return $ Right res
