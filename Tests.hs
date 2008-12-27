import Char
import List
import Test.HUnit
import Text.Printf
import Text.RegexC

main = runTestTT tests

tests = test [
    testStar,
    testDollar,
    testCaret ]

testStar = test [
    "test1" ~: Just "abab" ~=? regexMatch testRxStar "ababcd",
    "test2" ~: Nothing ~=? regexMatch testRxStar "acd",
    "test3" ~: Just "abababab" ~=? regexMatch testRxStar "ababababa" ]

testDollar = test [
    "test1" ~: Just "abab" ~=? regexMatch testRxDollar "abab" ]

testCaret = test [
    "test1" ~: Just "ab" ~=? regexMatch testRxCaret "abcd",
    "test2" ~: Nothing ~=? regexMatch testRxCaret "cdab",
    "test3" ~: Nothing ~=? regexMatch testRxCaret "" ]

-- m/(ab)*ab/
testRxStar :: Regex String
testRxStar = do
    (matched, _) <- rxStar $ do
        rxOneChar 'a'
        rxOneChar 'b'
    rxOneChar 'a'
    rxOneChar 'b'
    return matched

-- m/abab$/
testRxDollar :: Regex ()
testRxDollar = do
    rxOneChar 'a'
    rxOneChar 'b'
    rxOneChar 'a'
    rxOneChar 'b'
    rxDollar
    return ()

-- -- m/(ab)ab/
-- testRxParenthesis :: Regex String
-- testRxParenthesis = do
--     (matched, _) <- rxParenthesis $ do
--         rxOneChar 'a'
--         rxOneChar 'b'
--     rxOneChar 'a'
--     rxOneChar 'b'
--     rxDollar
--     return matched

-- m/^ab/
testRxCaret :: Regex ()
testRxCaret = do
    rxCaret
    rxOneChar 'a'
    rxOneChar 'b'
    return ()