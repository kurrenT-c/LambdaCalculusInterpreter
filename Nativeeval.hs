
module NativeEval (nativeNormalize) where

import Foreign.C.String (CString, newCString, peekCString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (free)

foreign import ccall safe "lc_normalize"
  c_lc_normalize :: CString -> CInt -> IO CString

foreign import ccall safe "lc_free"
  c_lc_free :: CString -> IO ()

nativeNormalize :: String -> Int -> IO String
nativeNormalize src maxSteps = do
  let asciiSrc = map (\c -> if c == 'λ' then '\\' else c) src
  csrc <- newCString asciiSrc
  cresult <- c_lc_normalize csrc (fromIntegral maxSteps)
  free csrc
  result <- peekCString cresult
  c_lc_free cresult
  pure result