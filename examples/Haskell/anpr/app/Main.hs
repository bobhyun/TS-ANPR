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

{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import TSANPR
import Data.ByteString qualified as BS
import Foreign
import System.Exit
import System.Directory (doesFileExist)

-- | examplesBaseDir: Root directory for examples and binaries
examplesBaseDir :: String
examplesBaseDir = "../.."

-- | getEngineFileName: Generate engine filename depending on platform/architecture
getEngineFileName :: String
getEngineFileName =
#ifdef mingw32_HOST_OS
#ifdef x86_64_HOST_ARCH
  -- 64-bit Windows
  examplesBaseDir ++ "\\bin\\windows-x86_64\\tsanpr.dll"
#else
  -- 32-bit Windows
  examplesBaseDir ++ "\\bin\\windows-x86\\tsanpr.dll"
#endif
#else
#ifdef x86_64_HOST_ARCH
  -- 64-bit Linux
  examplesBaseDir ++ "/bin/linux-x86_64/libtsanpr.so"
#elif defined(aarch64_HOST_ARCH)
  -- 64-bit ARM Linux
  examplesBaseDir ++ "/bin/linux-aarch64/libtsanpr.so"
#else
  ""
#endif
#endif

-- | Read image file and perform ANPR
readImageFile :: String -> String -> String -> IO ()
readImageFile imgFile outputFormat options = do
  putStr $ imgFile ++ " (outputFormat=\"" ++ outputFormat ++ "\", options=\"" ++ options ++ "\") => "
  result <- anprReadFile imgFile outputFormat options
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right res -> putStrLn res

-- | Read encoded image from file and perform ANPR
readEncodedImage :: String -> String -> String -> IO ()
readEncodedImage imgFile outputFormat options = do
  putStr $ imgFile ++ " (outputFormat=\"" ++ outputFormat ++ "\", options=\"" ++ options ++ "\") => "
  fileExists <- doesFileExist imgFile
  if not fileExists
    then putStrLn "File open failed"
    else do
      encodedImg <- BS.readFile imgFile
      BS.useAsCStringLen encodedImg $ \(ptr, len) -> do
        result <- anprReadPixels
          (castPtr ptr)
          (fromIntegral len)
          0
          0
          "encoded"
          outputFormat
          options
        case result of
          Left err -> putStrLn $ "Error: " ++ err
          Right res -> putStrLn res

-- | Read pixel buffer and perform ANPR using opencv-hs
readPixelBuffer :: String -> String -> String -> IO ()
readPixelBuffer imgFile outputFormat options = do
  putStr $ imgFile ++ " (outputFormat=\"" ++ outputFormat ++ "\", options=\"" ++ options ++ "\") => "
  -- For demonstration, we just print a warning
  putStrLn "readPixelBuffer: Not implemented in Haskell. Use OpenCV via FFI or the other image library."

-- | Read license plates for a specific country
readLicensePlates :: String -> IO Int
readLicensePlates countryCode = do
  -- NOTICE:
  -- anprInitialize should be called only once after library load.
  -- Therefore, it is not possible to change the country code after anprInitialize has been called.
  -- While using the free trial license, you can try all languages.
  -- When you purchase a commercial license, you can only use the selected language.
  let initParams = "text;country=" ++ countryCode
  initResult <- anprInitialize initParams
  case initResult of
    Left err -> do
      putStrLn $ "anpr_initialize() failed (error=" ++ err ++ ")"
      return (-1)
    Right _ -> do
      let imageDir = examplesBaseDir ++ "/img/" ++ countryCode ++ "/"
      -- TODO: Try each function as needed
      let anprFunc = readImageFile
      -- let anprFunc = readEncodedImage
      -- let anprFunc = readPixelBuffer
      -- TODO: Try each output format as needed
      let outputFormat = "text"
      -- let outputFormat = "json"
      -- let outputFormat = "yaml"
      -- let outputFormat = "xml"
      -- let outputFormat = "csv"
      anprFunc (imageDir ++ "licensePlate.jpg") outputFormat "" -- Single license plate recognition (default)
      anprFunc (imageDir ++ "multiple.jpg") outputFormat "vm" -- Recognize multiple license plates attached to vehicles
      anprFunc (imageDir ++ "multiple.jpg") outputFormat "vmb" -- Recognize multiple license plates including motorcycles
      anprFunc (imageDir ++ "surround.jpg") outputFormat "vms" -- Recognize multiple license plates with surround detection
      anprFunc (imageDir ++ "surround.jpg") outputFormat "dms" -- Recognize multiple surrounding objects (vehicles)
      anprFunc (imageDir ++ "surround.jpg") outputFormat "dmsr" -- Recognize multiple surrounding objects and license plates
      -- Recognize multiple surrounding objects and license plates within RoI
      anprFunc (imageDir ++ "surround.jpg") outputFormat "dmsri549,700,549,2427,1289,2427,1289,700"
      return 0

-- | Main function
main :: IO ()
main = do
  let engineFileName = getEngineFileName
  loadResult <- tsanprLoad engineFileName
  case loadResult of
    Left err -> do
      putStrLn $ "Failed to load TSANPR library: " ++ err
      exitWith (ExitFailure (-1))
    Right _ -> do
      -- TODO: Try each country code as needed
      result <- readLicensePlates "KR"
      -- result <- readLicensePlates "JP"
      -- result <- readLicensePlates "VN"
      tsanprUnload
      if result < 0
        then exitWith (ExitFailure result)
        else exitSuccess
