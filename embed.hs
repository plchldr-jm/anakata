import Data.List (isPrefixOf)

embed :: String -> String -> String

embed _ "" = ""

embed payload template 
  | "<script>\n" 
    `isPrefixOf` template
  = let 
    endT tpl
      | "\n</script>\n" 
        `isPrefixOf` tpl
      = tpl
      | otherwise
      = endT (tail tpl)
  in "<script>\n" 
  ++ payload 
  ++ endT template
  | otherwise
  = case template of
    (c:cs) -> 
      (c:embed payload cs)

--embedPL :: String -> String -> String