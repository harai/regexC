import Char
import List
import Test.HUnit
import Text.Printf
import Text.RegexC

main = runTestTT tests

tests = test [
    testZeroOrMoreLongest,
    testEnd,
    testCaret ]

testZeroOrMoreLongest = test [
    "test1" ~: Just "abab" ~=? regexMatch testRegex "ababcd",
    "test2" ~: Nothing ~=? regexMatch testRegex "acd" ]

testEnd = test [
    "test1" ~: Just "abab" ~=? regexMatch testRegex2 "abab" ]

testCaret = test [
    "test1" ~: Just "ab" ~=? regexMatch testRxCaret "abcd",
    "test2" ~: Nothing ~=? regexMatch testRxCaret "cdab",
    "test3" ~: Nothing ~=? regexMatch testRxCaret "" ]

-- m/(ab)*ab/
testRegex :: Regex String
testRegex = do
    (matched, _) <- rxZeroOrMoreLongest $ do
        rxOneChar 'a'
        rxOneChar 'b'
    rxOneChar 'a'
    rxOneChar 'b'
    return matched

-- m/(ab)ab$/
testRegex2 :: Regex String
testRegex2 = do
    (matched, _) <- rxOne $ do
        rxOneChar 'a'
        rxOneChar 'b'
    rxOneChar 'a'
    rxOneChar 'b'
    rxEnd
    return matched

-- m/^ab/
testRxCaret :: Regex ()
testRxCaret = do
    rxCaret
    rxOneChar 'a'
    rxOneChar 'b'
    return ()