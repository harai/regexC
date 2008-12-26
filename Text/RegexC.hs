{-# OPTIONS -fglasgow-exts #-}
module Text.RegexC where
 
newtype Regex a = Rx { runRegex :: (RxTarget -> [(RxTarget, a)]) }
    
-- rxTargetScanned and rxCurrentMatch hold scanned and matched chars in reverse order
data RxTarget = RxTarget {
    rxTargetToScan :: String,
    rxTargetScanned :: String,
    rxCurrentMatch :: String }

instance Monad Regex where
    m >>= next = Rx $ \target -> let
        thisCand = runRegex m target
        getNextCand (target', a) = runRegex (next a) target'
        in concatMap getNextCand thisCand
    return a = Rx $ \target -> [(target, a)]

rxOneChar :: Char -> Regex Char
rxOneChar c = Rx $ \target ->
    case target of
        RxTarget (x : toScan) scanned matched ->
            if c == x
            then [(RxTarget toScan (x : scanned) (x : matched), x)]
            else []
        _ -> []

-- http://www.haskell.org/ghc/docs/latest/html/users_guide/other-type-extensions.html#scoped-type-variables
-- returns matched string and an arbitrary return value, which is Nothing if zero-matched.
rxZeroOrMoreLongest :: forall a . Regex a -> Regex (String, Maybe a)
rxZeroOrMoreLongest sub =
    Rx $ \(RxTarget toScan scanned _) -> let
        getCand'' :: String -> (RxTarget, a) -> (RxTarget, (String, Maybe a))
        getCand'' prevMatch (RxTarget toScan' scanned' matched', a) =
            (RxTarget toScan' scanned' matched, (reverse matched, Just a))
            where
                matched = matched' ++ prevMatch

        getCand' :: [(RxTarget, (String, Maybe a))] -> [(RxTarget, (String, Maybe a))]
        getCand' ((target@(RxTarget _ _ matched), _) : _) =
            map (getCand'' matched) $ runRegex sub target
        getCand' [] = []

        getCand :: [(RxTarget, (String, Maybe a))]
        getCand =
            reverse $ map head $ takeWhile (not . null) $
            iterate getCand' [(RxTarget toScan scanned [], ("", Nothing))]
        in getCand

rxOne :: forall a . Regex a -> Regex (String, a)
rxOne sub = Rx $ \(RxTarget toScan scanned _)  -> let
    getCand' :: [(RxTarget, a)] -> [(RxTarget, (String, a))]
    getCand' = map (\(target'@(RxTarget _ _ matched), a) -> (target', (reverse matched, a)))

    getCand :: [(RxTarget, (String, a))]
    getCand = getCand' $ runRegex sub $ RxTarget toScan scanned []
    in getCand

rxCaret :: Regex ()
rxCaret = Rx $ \target ->
    case target of
        RxTarget toScan [] matched -> [(RxTarget toScan [] matched, ())]
        _ -> []

rxEnd :: Regex ()
rxEnd = Rx $ \target ->
    case target of
        RxTarget [] scanned matched -> [(RxTarget [] scanned matched, ())]
        _ -> []

regexMatch :: Regex a -> String -> Maybe String
regexMatch rx str =
    case runRegex (rxOne rx) (RxTarget str [] []) of
        [] -> Nothing
        (RxTarget _ _ matched, _) : _ -> Just (reverse matched)

-- regexReplace :: Regex a -> String -> String
-- regexReplace
