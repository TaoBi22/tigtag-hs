module TagTypes where

data Rule = Rule {name :: String, triggerchar :: Char, numbertodrop :: Integer, printchars :: Bool, append_chars :: String} 
