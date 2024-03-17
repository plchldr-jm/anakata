import Data.List (isPrefixOf)

extract :: String -> String

extract [] 
  = []

extract cartridge 
  | "<script>\n" 
    `isPrefixOf` cartridge
  = let 
    code crt
      | "\n</script>\n" 
        `isPrefixOf` crt
      = ""
      | otherwise
      = case crt of 
        (c:cs) -> 
          (c:code cs)
  in code . drop 9 $ cartridge
  | otherwise
  = extract . tail $ cartridge

--extractPL :: String -> String -> String