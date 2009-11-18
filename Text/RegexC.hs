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
rxOneChar c = Rx $ \target ->
    case target of
        RxTarget _ _ (x : _) ->
            if c == x
            then [(afterToMatched target, x)]
            else []
        _ -> []

rxDot :: Regex Char
rxDot = Rx $ \target ->
    case target of
        RxTarget _ _ (x : _) -> [(afterToMatched target, x)]
        _ -> []

rxBracket :: String -> Regex Char
rxBracket chars = Rx $ \target ->
    case target of
        RxTarget _ _ (x : _) ->
            if x `elem` chars
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
    case runRegex regex (RxTarget [] [] str) of
        [] -> Nothing
        (_, matched) : _ -> Just matched
    where
       regex = do
           rxStarQ rxDot
           (matched, _) <- rxParenthesis rx
           return matched
-- regexReplace :: Regex a -> String -> String
-- regexReplace
