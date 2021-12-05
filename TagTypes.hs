module TagTypes where

data Rule = Rule {name :: String, triggerchar :: Char, numbertodrop :: Int, printchars :: Bool, append_chars :: String} 
