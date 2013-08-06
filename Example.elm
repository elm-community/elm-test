module Main where

import open ElmTest

-- Example Usage
----------------

main = runPrettyTests <| [ (2^3) ~=? 1
                         , 3 ~=? 3
                         , defaultTest (True @=? True)
                         , test "test head" (assertEqual 1 (head [1..10]))
                         ]
                         ++
                         (generateFunctionTests "Square" 
                                                (\x -> if (x < 4) then x^2 else x^3)
                                                [1..5]
                                                [1,4,9,16,25])