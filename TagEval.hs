import TagTypes

-- This function will fully run through a queue, but can only take one rule - currently only left in for reference
tagEval1r :: String -> Rule -> String
tagEval1r "" _ = ""
tagEval1r (x:xs) rule = if x == triggerchar rule then
                          tagEval1r (removeStartChars xs (numbertodrop rule - 1) ++ (append_chars rule)) rule
                        else
                          (x:xs)


-- Runs one iteration of Tag evaluation on a queue - as soon as it finds an applicable rule it returns the result
tagEvalOnce :: String -> [Rule] -> String
tagEvalOnce "" _ = ""
tagEvalOnce (x:xs) [] = xs
tagEvalOnce (x:xs) (r:rs) = if x == triggerchar r then
                              (removeStartChars xs (numbertodrop r - 1) ++ (append_chars r))
                            else
                             tagEvalOnce (x:xs) rs

tagEval :: String -> [Rule] -> String
tagEval "" _ = ""
tagEval x [] = x
tagEval ('#':xs) _ = xs -- # is our halting character so we just return the stack remainder as the result
tagEval x r = tagEval (tagEvalOnce x r) r -- Evaluate the string according to the rules once, and recurse again


removeStartChars :: String -> Integer -> String
removeStartChars x 0 = x
removeStartChars "" _ = ""
removeStartChars (x:xs) n = if n > 1 then
                              removeStartChars xs (n-1)
                            else (x:xs)
