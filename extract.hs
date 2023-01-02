import Data.List (isPrefixOf)

extract :: String -> String -> String

extract _ "" = ""

extract payload template 
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
      (c:extract payload cs)

--extractPL :: String -> String -> String