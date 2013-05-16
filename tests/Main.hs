
import Test.Framework (defaultMain, testGroup)

import qualified Modules.Database as D

main :: IO ()
main = do putStrLn $ "Running Tests"
          defaultMain tests

tests = [ testGroup "Database" D.tests
        ]
        -- testGroup "Name" SymLens.tests