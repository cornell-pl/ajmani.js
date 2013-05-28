
import Test.Framework (defaultMain, testGroup, Test)

import qualified Modules.Database as D
import qualified Modules.SymLens as SL

main :: IO ()
main = do
  putStrLn $ "Running Tests"
  defaultMain tests

tests :: [Test]
tests = [ testGroup "Database" D.tests
        , testGroup "SymLens" SL.tests
        ]
        -- testGroup "Name" SymLens.tests
