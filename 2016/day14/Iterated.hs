module Iterated (iterated) where
import           Data.ByteString.Char8 (ByteString, pack, unpack)
import           Crypto.Hash
md5 :: ByteString -> Digest MD5
md5 = hash

iterated :: ByteString -> ByteString
iterated = once
  . foldr (.) id (replicate 252 woopwoop)

once :: ByteString -> ByteString
once = pack . show . md5
{-# INLINE once #-}

woopwoop :: ByteString -> ByteString
woopwoop = once . once . once . once
  . once . once . once . once
{-# INLINE woopwoop #-}
