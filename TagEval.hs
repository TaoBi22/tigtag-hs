import TagTypes

tagEval :: String -> Rule -> String
tagEval "" _ = ""
tagEval (x:xs) rule = if x == triggerchar rule then
			tagEval (removeStartChars xs (numbertodrop rule - 1) ++ (append_chars rule)) rule
		    else
			(x:xs)	
		

removeStartChars :: String -> Integer -> String
removeStartChars x 0 = x
removeStartChars "" _ = ""
removeStartChars (x:xs) n = if n > 1 then
				removeStartChars xs (n-1)	
			else (x:xs)
