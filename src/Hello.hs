-- creates module "Hello" which exports the function "testPrint"
-- will be useable if we "import qualified Hello (testPrint)" in our Main.hs
-- inside app.
module Hello (testPrint) where

testPrint :: IO ()
testPrint = putStrLn "Hello, world!"
