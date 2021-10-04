module TagTypes where

data Rule = Rule {name :: String, triggerchar :: Char, numbertodrop :: Integer, append_chars :: String} 
