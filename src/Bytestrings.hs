-- BYTESTRINGS
-- http://learnyouahaskell.com/input-and-output
-- why are bytestrings used?
-- - processing files with strings --> [Char] is slow since it is lazy and reads
-- input one by one char.
-- Bytestrings are used to reading/manipulating big files. They come in
-- LAZY
-- - (error prone.. e.g.: could read in file, close handle, do something with
-- result from read --> will result in an error... because it was not read
-- because it was not used before closing the handle..)
-- and NOT LAZY. (e.g. read everything when we tell haskell to read a file)
-- - can be faster, consume less memory

-- has a lot of common with lists. Only the types are Bytestring isntead of [a]
-- and Word8 instead of a. Word8 is Num type with values 0-255. (8 bit possible
-- values)

-- import qualified Data.Bytestring as ByteString
-- import qualified Data.Bytestring.Lazy as LazyByteString
module Bytestrings (testPack) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LazyBS

-- NOTE: how can I add bytestring package?
-- - added it in my .cabal file
-- - ran cabal install bytestring

-- pack :: [Word8] -> ByteString --> takes list of values within rrange 0-255
-- and returns a bytestring. (e.g. takes lazy and returns less lazy)
testPack = BS.pack [99, 97, 110]

-- NOTE: can use bytestrings whenever we need more performance.
-- Can just use bytestring instead of default system io.
-- example "
-- import System.Environment
-- import qualified Data.ByteString.Lazy as B
--
-- main = do
--     (fileName1:fileName2:_) <- getArgs
--     copyFile fileName1 fileName2
--
-- copyFile :: FilePath -> FilePath -> IO ()
-- copyFile source dest = do
--     contents <- B.readFile source
--     B.writeFile dest contents
--     "
--
--     CONTINUE Exceptions.hs
