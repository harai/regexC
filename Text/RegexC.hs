{-# OPTIONS -fglasgow-exts #-}
module Text.RegexC where
 
newtype Regex a = Rx { runRegex :: (RxTarget -> [(RxTarget, a)]) }
    
-- rxTargetBefore and rxTargetMatched hold scanned and matched chars in reverse order
data RxTarget = RxTarget {
    rxTargetBefore :: String,
    rxTargetMatched :: String,
    rxTargetAfter :: String }

afterToMatched :: RxTarget -> RxTarget
afterToMatched (RxTarget bs ms (a : as)) = RxTarget bs (a : ms) as
afterToMatched (RxTarget _ _ _) = error "Should not happen."

matchedToBeforeAll :: RxTarget -> RxTarget
matchedToBeforeAll (RxTarget bs ms as) = RxTarget (reverse ms ++ bs) [] as

recoverTarget :: RxTarget -> RxTarget -> String -> RxTarget
recoverTarget start returned matchedHere =
    RxTarget (rxTargetBefore start) (matchedHere ++ rxTargetMatched start) (rxTargetAfter returned)

instance Monad Regex where
    m >>= next = Rx $ \target -> let
        thisCand = runRegex m target
        getNextCand (target', a) = runRegex (next a) target'
        in concatMap getNextCand thisCand
    return a = Rx $ \target -> [(target, a)]

instance Functor Regex where
    fmap f rx = Rx $ \target -> map (\(t, v) -> (t, f v)) $ runRegex rx target

rxOneChar :: Char -> Regex Char
rxOneChar c = rxChar (== c)

rxDot :: Regex Char
rxDot = rxChar $ \_ -> True

rxBracket :: String -> Regex Char
rxBracket chars = rxChar (`elem` chars)

rxChar :: (Char -> Bool) -> Regex Char
rxChar isElement = Rx $ \target ->
    case target of
        RxTarget _ _ (x : _) ->
            if isElement x
            then [(afterToMatched target, x)]
            else []
        _ -> []

rxStar :: Regex a -> Regex (String, Maybe a)
rxStar = makeGroup reverse

rxStarQ :: Regex a -> Regex (String, Maybe a)
rxStarQ = makeGroup id

rxParenthesis :: forall a . Regex a -> Regex (String, a)
rxParenthesis rx = fmap nomaybe $ makeGroup (take 1 . drop 1) rx
    where
        nomaybe :: (String, Maybe a) -> (String, a)
        nomaybe (s, Just a) = (s, a)
        nomaybe (s, Nothing) = error "Should not happen."

rxPipe :: Regex a -> Regex b -> Regex (String, Either a b)
rxPipe rx1 rx2 = Rx $ \target ->
    case runRegex (rxParenthesis rx1) target of
        (t, (matched, ret)) : [] -> [(t, (matched, Left ret))]
        [] ->
            case runRegex (rxParenthesis rx2) target of
                (t, (matched, ret)) : [] -> [(t, (matched, Right ret))]
                [] -> []
                _ -> error "Should not happen."
        _ -> error "Should not happen."

rxPipe3 :: Regex a -> Regex a -> Regex a -> Regex (String, a)
rxPipe3 rx1 rx2 rx3 = do
    (matched, ret) <- rxPipe rx1 $ rxPipe rx2 rx3
    case ret of
        Left a -> return (matched, a)
        Right (_, Left a) -> return (matched, a)
        Right (_, Right a) -> return (matched, a)

rxPipe3_ :: Regex a -> Regex b -> Regex c -> Regex String
rxPipe3_ rx1 rx2 rx3 = do
    (matched, _) <- rxPipe rx1 $ rxPipe rx2 rx3
    return matched

-- http://www.haskell.org/ghc/docs/latest/html/users_guide/other-type-extensions.html#scoped-type-variables
-- returns matched string and an arbitrary return value, which is Nothing if zero-matched.
makeGroup :: forall a . (forall b . [b] -> [b]) -> Regex a -> Regex (String, Maybe a)
makeGroup sortFunc sub =
    Rx $ \target -> let
        getCand' :: [(RxTarget, (String, Maybe a))] -> [(RxTarget, (String, Maybe a))]
        getCand' ((target', (matchedHere, _)) : _) =
            map (\(subTgt, a) -> (subTgt, (rxTargetMatched subTgt ++ matchedHere, Just a))) $
            runRegex sub $ matchedToBeforeAll target'
        getCand' [] = []

        getCand :: [(RxTarget, (String, Maybe a))]
        getCand =
            map (\(t, (str, ret)) -> (recoverTarget target t str, (reverse str, ret))) $
            sortFunc $ map head $ takeWhile (not . null) $
            iterate getCand' [(target, ([], Nothing))]
        
        in getCand

rxCaret :: Regex ()
rxCaret = Rx $ \target ->
    case target of
        RxTarget [] _ _ -> [(target, ())]
        _ -> []

rxDollar :: Regex ()
rxDollar = Rx $ \target ->
    case target of
        RxTarget _ _ [] -> [(target, ())]
        _ -> []

regexMatch :: Regex a -> String -> Maybe String
regexMatch rx str =
    case regexBase rx str of
        Nothing -> Nothing
        Just (_, matched, _, _) -> Just matched

-- regexMatch :: Regex a -> String -> Maybe String
-- regexMatch rx str =
--     case runRegex regex (RxTarget [] [] str) of
--         [] -> Nothing
--         (_, matched) : _ -> Just matched
--     where
--        regex = do
--            rxStarQ rxDot
--            (matched, _) <- rxParenthesis rx
--            return matched

-- returns (before, matched, after, returnedValue)
regexBase :: Regex a -> String -> Maybe (String, String, String, a)
regexBase rx str =
    case runRegex regex (RxTarget [] [] str) of
        [] -> Nothing
        (RxTarget _ _ after, (before, matched, ret)) : _ -> Just (before, matched, after, ret)
    where
       regex = do
           (before, _) <- rxStarQ rxDot
           (matched, ret) <- rxParenthesis rx
           return (before, matched, ret)
