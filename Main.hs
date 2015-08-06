{-# LANGUAGE OverloadedStrings #-}
module Main where

import Codec.Crypto.RSA.Pure
import Control.Applicative ((<$>), (<*>))
import Control.Lens ((^.))
import Crypto.Random (SystemRandom(), newGenIO)
import Data.Binary.Get (getLazyByteString, runGet)
import Data.ByteString.Base64.Lazy (decode, encode)
import Data.ByteString.Lazy (ByteString())
import Data.ByteString.Lazy.Char8 (lines, putStrLn, words)
import Data.Digest.Pure.SHA (bytestringDigest, sha1)
import Data.Foldable (traverse_)
import Data.Maybe (catMaybes, listToMaybe)
import Network.Wreq (get, responseBody)

import Prelude hiding (lines, putStrLn, words)

readSshPublicKey :: ByteString -> PublicKey
readSshPublicKey = runGet $ makePublicKey <$> readBS <*> readI <*> readI
  where readBS = getLazyByteString 4 >>= getLazyByteString . fromIntegral . os2ip
        readI = os2ip <$> readBS
        makePublicKey _ e n = PublicKey (calculateModulus n 1) n e

-- From RSA library
calculateModulus :: Integer -> Int -> Int
calculateModulus n i = if (2 ^ (i * 8)) > n then i else calculateModulus n (i+1)

type Username = String

fetchPublicKeys :: Username -> IO [Either String PublicKey]
fetchPublicKeys username = run <$> get ("https://github.com/" ++ username ++ ".keys")
  where run r = fmap getKey . catMaybes $ listToMaybe . drop 1 . words <$> lines (r ^. responseBody)
        getKey = fmap readSshPublicKey . decode

say :: SystemRandom -> Either String PublicKey -> IO ()
say g = either print $ either print (putStrLn . encode . fst) . oaep "Hello"
  where hash = bytestringDigest . sha1
        oaep = flip $ encryptOAEP g hash (generateMGF1 hash) ""

main :: IO ()
main = do
  g <- newGenIO
  fetchPublicKeys "puffnfresh" >>= traverse_ (say g)
