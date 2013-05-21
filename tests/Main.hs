
import Test.Framework (defaultMain, testGroup)

import qualified Modules.Database as D
import qualified Modules.SymLens as SL

import Test.QuickCheck

main :: IO ()
main = do
  putStrLn $ "Running Tests"
  defaultMain tests

tests = [ testGroup "Database" D.tests
        , testGroup "SymLens" SL.tests
        ]
        -- testGroup "Name" SymLens.tests