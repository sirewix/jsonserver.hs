module Test.Common where

import           Test.QuickCheck                ( Testable
                                                , quickCheck
                                                )

signedCheck :: Testable prop => String -> prop -> IO ()
signedCheck msg f = do
  putStrLn $ "Testing " <> msg <> " "
  quickCheck f
