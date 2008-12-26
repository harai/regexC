{-# OPTIONS -fglasgow-exts #-}
module Text.RegexC where
 
newtype Regex a = Rx { runRegex :: (RxTarget -> [(RxTarget, a)]) }
    
-- rxTargetScanned holds scanned chars in reverse order
data RxTarget = RxTarget { rxTargetToScan :: String, rxTargetScanned :: String }

instance Monad Regex where
    m >>= next = Rx $ \target -> let
                           thisCand = runRegex m target
                           getNextCand (target', a) = runRegex (next a) target'
                      in concatMap getNextCand thisCand
    return a = Rx $ \target -> [(target, a)]

rxOneChar :: Char -> Regex Char
rxOneChar c = Rx $ \target -> case target of
                         RxTarget (x : xs) ys -> if c == x
                                                then [(RxTarget xs (x : ys), x)]
                                                else []
                         _ -> []

-- http://www.haskell.org/ghc/docs/latest/html/users_guide/other-type-extensions.html#scoped-type-variables
-- returns matched string and an arbitrary return value, which is Nothing if zero-matched.
rxZeroOrMoreLongest :: forall a . Regex a -> Regex (String, Maybe a)
rxZeroOrMoreLongest sub =
    Rx $ \target -> let
        getCand' :: [(RxTarget, (String, Maybe a))] -> [(RxTarget, (String, Maybe a))]
        getCand' ((target', (matchedStr, _)) : _) =
            map (\(t, a) -> (t, (matchedStr ++ getConsumed target' t, Just a))) $ runRegex sub target'
        getCand' [] = []

        getCand :: [(RxTarget, (String, Maybe a))]
        getCand = reverse $
                  map head $
                  takeWhile (not . null) $
                  iterate getCand' [(target, ("", Nothing))]
        in getCand

rxOne :: forall a . Regex a -> Regex (String, a)
rxOne sub =
    Rx $ \target -> let
        getCand' :: [(RxTarget, a)] -> [(RxTarget, (String, a))]
        getCand' = map (\(target', a) -> (target', (getConsumed target target', a)))

        getCand :: [(RxTarget, (String, a))]
        getCand = getCand' $ runRegex sub target
        in getCand

rxCaret :: Regex ()
rxCaret = Rx $ \target ->
    case target of
        RxTarget xs [] -> [(RxTarget xs [], ())]
        _ -> []

rxEnd :: Regex ()
rxEnd = Rx $ \target -> case target of
                       RxTarget [] ys -> [(RxTarget [] ys, ())]
                       _ -> []

-- 暫定的なコード
getConsumed :: RxTarget -> RxTarget -> String
getConsumed (RxTarget before _) (RxTarget after _) = reverse $ drop (length after) $ reverse before

regexMatch :: Regex a -> String -> Maybe String
regexMatch rx str = case runRegex (rxOne rx) (RxTarget str []) of
                      [] -> Nothing
                      (_, (matchedStr, _)) : _ -> Just matchedStr

-- regexReplace :: Regex a -> String -> String
-- regexReplace
