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
afterToMatched (RxTarget _ _ _) = error "Should not be executed."

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

rxOneChar :: Char -> Regex Char
rxOneChar c = Rx $ \target ->
    case target of
        RxTarget _ _ (x : _) ->
            if c == x
            then [(afterToMatched target, x)]
            else []
        _ -> []

-- http://www.haskell.org/ghc/docs/latest/html/users_guide/other-type-extensions.html#scoped-type-variables
-- returns matched string and an arbitrary return value, which is Nothing if zero-matched.
rxStar :: forall a . Regex a -> Regex (String, Maybe a)
rxStar sub =
    Rx $ \target -> let
        getCand' :: [(RxTarget, (String, Maybe a))] -> [(RxTarget, (String, Maybe a))]
        getCand' ((target', (matchedHere, _)) : _) =
            map (\(subTgt, a) -> (subTgt, (rxTargetMatched subTgt ++ matchedHere, Just a))) $
            runRegex sub $ matchedToBeforeAll target'
        getCand' [] = []

        getCand :: [(RxTarget, (String, Maybe a))]
        getCand =
            map (\(t, (str, ret)) -> (recoverTarget target t str, (reverse str, ret))) $
            reverse $ map head $ takeWhile (not . null) $
            iterate getCand' [(target, ([], Nothing))]
        
        in getCand

rxParenthesis :: forall a . Regex a -> Regex (String, a)
rxParenthesis sub = Rx $ \target -> let
    getCand' :: (RxTarget, a) -> (RxTarget, (String, a))
    getCand' (t, a) = (recoverTarget target t (rxTargetMatched t), (reverse $ rxTargetMatched t, a))

    getCand :: [(RxTarget, (String, a))]
    getCand = map getCand' $ runRegex sub $ matchedToBeforeAll target
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
    case runRegex (rxParenthesis rx) (RxTarget [] [] str) of
        [] -> Nothing
        (RxTarget _ matched _, _) : _ -> Just (reverse matched)

-- regexReplace :: Regex a -> String -> String
-- regexReplace
