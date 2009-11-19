import Char
import List
import Test.HUnit
import Text.Printf
import Text.RegexC

main = runTestTT tests

tests = test [
    testStar,
    testDollar,
    testCaret,
    testBracket,
    testPipe,
    testPipe3 ]

testStar = test [
    "test1" ~: Just "abab" ~=? regexMatch testRxStar "ababcd",
    "test2" ~: Nothing ~=? regexMatch testRxStar "acd",
    "test3" ~: Just "abababab" ~=? regexMatch testRxStar "ababababa" ]

testDollar = test [
    "test1" ~: Just "abab" ~=? regexMatch testRxDollar "abab",
    "test2" ~: Just "abab" ~=? regexMatch testRxDollar "ababab",
    "test3" ~: Just "abab" ~=? regexMatch testRxDollar "cdabab",
    "test4" ~: Nothing ~=? regexMatch testRxDollar "abcdab" ]

testCaret = test [
    "test1" ~: Just "ab" ~=? regexMatch testRxCaret "abcd",
    "test2" ~: Nothing ~=? regexMatch testRxCaret "cdab",
    "test3" ~: Nothing ~=? regexMatch testRxCaret "" ]

testBracket = test [
    "test1" ~: Just "a" ~=? regexMatch testRxBracket "a",
    "test2" ~: Just "c" ~=? regexMatch testRxBracket "c",
    "test3" ~: Nothing ~=? regexMatch testRxBracket "d",
    "test4" ~: Nothing ~=? regexMatch testRxBracket "" ]

testPipe = test [
    "test1" ~: Just "one" ~=? regexMatch testRxPipe "hogeone",
    "test2" ~: Just "two" ~=? regexMatch testRxPipe "two",
    "test3" ~: Nothing ~=? regexMatch testRxPipe "hoge",
    "test4" ~: Nothing ~=? regexMatch testRxPipe "" ]

testPipe3 = test [
    "test1" ~: Just "one" ~=? regexMatch testRxPipe3 "hogeones",
    "test2" ~: Just "two" ~=? regexMatch testRxPipe3 "two",
    "test3" ~: Nothing ~=? regexMatch testRxPipe3 "hoge",
    "test4" ~: Nothing ~=? regexMatch testRxPipe3 "" ]

-- /(ab)*ab/
testRxStar :: Regex String
testRxStar = do
    (matched, _) <- rxStar $ do
        rxOneChar 'a'
        rxOneChar 'b'
    rxOneChar 'a'
    rxOneChar 'b'
    return matched

-- /[abc]/
testRxBracket :: Regex ()
testRxBracket = do
    rxBracket "abc"
    return ()

-- /(one|two)/
testRxPipe :: Regex ()
testRxPipe = do
    (matched, ret) <- rxPipe testResOne testResTwo
    case ret of
        Left 'e' -> return ()
        Right 'o' -> return ()
        _ -> error "test failed."

testResOne = do
    rxOneChar 'o'
    rxOneChar 'n'
    rxOneChar 'e'

testResOnes = do
    rxOneChar 'o'
    rxOneChar 'n'
    rxOneChar 'e'
    rxOneChar 's'

testResTwo = do
    rxOneChar 't'
    rxOneChar 'w'
    rxOneChar 'o'

testRxPipe3 :: Regex ()
testRxPipe3 = do
    (matched, ret) <- rxPipe3 testResOne testResOnes testResTwo
    return ()
                      
-- /abab$/
testRxDollar :: Regex ()
testRxDollar = do
    rxOneChar 'a'
    rxOneChar 'b'
    rxOneChar 'a'
    rxOneChar 'b'
    rxDollar
    return ()

-- -- /(ab)ab/
-- testRxParenthesis :: Regex String
-- testRxParenthesis = do
--     (matched, _) <- rxParenthesis $ do
--         rxOneChar 'a'
--         rxOneChar 'b'
--     rxOneChar 'a'
--     rxOneChar 'b'
--     rxDollar
--     return matched

-- /^ab/
testRxCaret :: Regex ()
testRxCaret = do
    rxCaret
    rxOneChar 'a'
    rxOneChar 'b'
    return ()