{-# OPTIONS -fglasgow-exts #-}
module Text.RegexC where
 
newtype Regex a = Rx { runRegex :: (RxTarget -> [(RxTarget, a)]) }
    
-- rxTargetScanned and rxCurrentMatch hold scanned and matched chars in reverse order
-- (reverse rxTargetScanned t) ++ (reverse rxCurrentMatch t) ++ (rxTargetToScan t) == targetStr
data RxTarget = RxTarget {
    rxTargetScanned :: String,
    rxCurrentMatch :: String,
    rxTargetToScan :: String }

instance Monad Regex where
    m >>= next = Rx $ \target -> let
        thisCand = runRegex m target
        getNextCand (target', a) = runRegex (next a) target'
        in concatMap getNextCand thisCand
    return a = Rx $ \target -> [(target, a)]

rxOneChar :: Char -> Regex Char
rxOneChar c = Rx $ \target ->
    case target of
        RxTarget scanned matched (x : toScan) ->
            if c == x
            then [(RxTarget (x : scanned) (x : matched) toScan, x)]
            else []
        _ -> []

-- http://www.haskell.org/ghc/docs/latest/html/users_guide/other-type-extensions.html#scoped-type-variables
-- returns matched string and an arbitrary return value, which is Nothing if zero-matched.
rxStar :: forall a . Regex a -> Regex (String, Maybe a)
rxStar sub =
    Rx $ \(RxTarget scanned _ toScan) -> let
        getCand'' :: String -> (RxTarget, a) -> (RxTarget, (String, Maybe a))
        getCand'' prevMatch (RxTarget scanned' matched' toScan', a) =
            (RxTarget scanned' matched toScan', (reverse matched, Just a))
            where
                matched = matched' ++ prevMatch

        getCand' :: [(RxTarget, (String, Maybe a))] -> [(RxTarget, (String, Maybe a))]
        getCand' ((target@(RxTarget _ matched _), _) : _) =
            map (getCand'' matched) $ runRegex sub target
        getCand' [] = []

        getCand :: [(RxTarget, (String, Maybe a))]
        getCand =
            reverse $ map head $ takeWhile (not . null) $
            iterate getCand' [(RxTarget scanned [] toScan, ("", Nothing))]
        in getCand

rxParenthesis :: forall a . Regex a -> Regex (String, a)
rxParenthesis sub = Rx $ \(RxTarget scanned _ toScan)  -> let
    getCand' :: [(RxTarget, a)] -> [(RxTarget, (String, a))]
    getCand' = map (\(target'@(RxTarget _ matched _), a) -> (target', (reverse matched, a)))

    getCand :: [(RxTarget, (String, a))]
    getCand = getCand' $ runRegex sub $ RxTarget scanned [] toScan
    in getCand

rxCaret :: Regex ()
rxCaret = Rx $ \target ->
    case target of
        RxTarget [] matched toScan -> [(RxTarget [] matched toScan, ())]
        _ -> []

rxDollar :: Regex ()
rxDollar = Rx $ \target ->
    case target of
        RxTarget scanned matched [] -> [(RxTarget scanned matched [], ())]
        _ -> []

regexMatch :: Regex a -> String -> Maybe String
regexMatch rx str =
    case runRegex (rxParenthesis rx) (RxTarget [] [] str) of
        [] -> Nothing
        (RxTarget _ matched _, _) : _ -> Just (reverse matched)

-- regexReplace :: Regex a -> String -> String
-- regexReplace
