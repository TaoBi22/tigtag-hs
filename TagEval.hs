import TagTypes

-- This function will fully run through a queue, but can only take one rule - currently only left in for reference
tagEval1r :: String -> Rule -> String
tagEval1r "" _ = ""
tagEval1r (x:xs) rule = if x == triggerchar rule then
                          tagEval1r (drop (numbertodrop rule - 1) xs ++ (append_chars rule)) rule
                        else
                          (x:xs)


-- Runs one iteration of Tag evaluation on a queue - as soon as it finds an applicable rule it returns the result
tagEvalOnce :: String -> [Rule] -> (String, String)
tagEvalOnce "" _ = ("", "")
tagEvalOnce (x:xs) [] = (xs, "")
tagEvalOnce (x:xs) (r:rs) = if x == triggerchar r then
                              let printstr = if printchars r then take (fromIntegral (numbertodrop r - 1)) xs else "" in
                              (drop (numbertodrop r - 1) xs ++ (append_chars r), printstr)
                            else
                             tagEvalOnce (x:xs) rs

tagEval :: String -> [Rule] -> IO String
tagEval "" _ = do return ""
tagEval x [] = do return x
tagEval ('#':xs) _ = do return xs -- # is our halting character so we just return the stack remainder as the result
tagEval x r = do putStr printstr; (tagEval stackstr r)
              where (stackstr, printstr) = (tagEvalOnce x r) -- Evaluate the string according to the rules once, and recurse again
