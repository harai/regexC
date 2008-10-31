{-# OPTIONS -fglasgow-exts #-}
module RegexC where
 
newtype Regex a = Rx { runRegex :: (String -> [(String, a)]) }
--newtype RxInput = RxIn { rxInToMatch :: String, rxInMatched :: String } -- MatchをScanに

instance Monad Regex where
    m >>= next = Rx $ \str -> let
                           thisCand = runRegex m str
                           getNextCand (str', a) = runRegex (next a) str'
                      in concatMap getNextCand thisCand
    return a = Rx $ \str -> [(str, a)]

rxOneChar :: Char -> Regex Char
rxOneChar c = Rx $ \str -> case str of
                         s : xs -> if c == s then [(xs, s)] else []
                         [] -> []

-- http://www.haskell.org/ghc/docs/latest/html/users_guide/other-type-extensions.html#scoped-type-variables
rxZeroOrMoreLongest :: forall a . Regex a -> Regex (String, Maybe a)
rxZeroOrMoreLongest sub =
    Rx $ \str -> let
        getCand' :: [(String, (String, Maybe a))] -> [(String, (String, Maybe a))]
        getCand' ((str', (matchedStr, _)) : _) =
            map (\(s, a) -> (s, (matchedStr ++ getConsumed str' s, Just a))) $ runRegex sub str'
        getCand' [] = []

        getCand :: [(String, (String, Maybe a))]
        getCand = reverse $
                  map head $
                  takeWhile (not . null) $
                  iterate getCand' [(str, ("", Nothing))]
        in getCand

rxOne :: forall a . Regex a -> Regex (String, a)
rxOne sub =
    Rx $ \str -> let
        getCand' :: [(String, a)] -> [(String, (String, a))]
        getCand' = map (\(str', a) -> (str', (getConsumed str str', a)))

        getCand :: [(String, (String, a))]
        getCand = getCand' $ runRegex sub str
        in getCand

rxEnd :: Regex ()
rxEnd = Rx $ \str -> case str of
                       [] -> [("", ())]
                       _ -> []

-- 暫定的なコード
getConsumed :: String -> String -> String
getConsumed before after = reverse $ drop (length after) $ reverse before


regexMatch :: Regex a -> String -> Bool
regexMatch rx = not . null . runRegex rx
           
-- regexReplace :: Regex a -> String -> String
-- regexReplace

-- /(ab)*ab/
testRegex :: Regex String
testRegex = do
  (matched, _) <- rxZeroOrMoreLongest $ do
                    rxOneChar 'a'
                    rxOneChar 'b'
  rxOneChar 'a'
  rxOneChar 'b'
  return matched

-- /(ab)ab$/
testRegex2 :: Regex String
testRegex2 = do
  (matched, _) <- rxOne $ do
                    rxOneChar 'a'
                    rxOneChar 'b'
  rxOneChar 'a'
  rxOneChar 'b'
  rxEnd
  return matched
