module ParserSpec (parserTests) where

import Test.HUnit
import ParserType (Parser (..))
import PositionType (Position(..), defaultPosition, moveCursor)
import ParserBool
import ParserChar
import ParserString
import ParserInt
import ParserCombinator
import SyntaxParser

parserTests :: Test
parserTests = TestList
          [ "defaultPosition" ~: defaultPositionTest
          , "moveCursor" ~: moveCursorTests
          , "parseChar" ~: parseCharTests
          , "parseAnyChar" ~: parseAnyCharTests
          , "parseOr" ~: parseOrTests
          , "parseAnd" ~: parseAndTests
          , "parseMany" ~: parseManyTests
          , "parseSome" ~: parseSomeTests
          , "parseDigit" ~: parseDigitTests
          , "parseUInt" ~: parseUIntTests
          , "parseNegInt" ~: parseNegIntTests
          , "parseInt" ~: parseIntTests
          , "parseWithSpace" ~: parseWithSpaceTests
          , "parseOpeningParenthesis" ~: parseOpeningParenthesisTests
          , "parseClosingParenthesis" ~: parseClosingParenthesisTests
          , "parsePair" ~: parsePairTests
          , "parseList" ~: parseListTests
          ]

getPosition :: Int -> Int -> Position
getPosition x y = Position {line = y, char = x}

defaultPositionTest :: Test
defaultPositionTest = TestCase $ assertEqual "default position is line = 0, char = 0" (getPosition 0 0) defaultPosition

moveCursorTests :: Test
moveCursorTests = TestList
  [ "Move cursor to the next char" ~: (getPosition 0 1) @=? (moveCursor (getPosition 0 0) True)
  , "Move cursor to the next line" ~: (getPosition 1 0) @=? (moveCursor (getPosition 0 0) False)
  ]

parseCharTests :: Test -- à completer
parseCharTests = TestList
  [ "delete the first character '\n'" ~: (Right ('\n', "hello world!", (getPosition 0 1))) @=? (runParser (parseChar '\n') "\nhello world!" defaultPosition)
  ]

parseAnyCharTests :: Test -- à completer
parseAnyCharTests = TestList
  [ "check if first char is in '\nc'" ~: (Right ('c',"oucou\n", (getPosition 1 0))) @=? (runParser (parseAnyChar "\nc") "coucou\n" defaultPosition)
  , "check if first char is in 'c\n'" ~: (Right ('c',"oucou\n", (getPosition 1 0))) @=? (runParser (parseAnyChar "c\n") "coucou\n" defaultPosition)
  , "check if first char is in 'rem'" ~: (Left ("Char not found in list", (getPosition 0 0))) @=? (runParser (parseAnyChar "re" ) "zero\n" defaultPosition)
  ]

parseOrTests :: Test
parseOrTests = TestList
  [ "Test 1" ~: (Right ('(',"hello world)",(getPosition 1 0))) @=? (runParser (parseOr (parseChar '(') (parseChar 'f')) "(hello world)" defaultPosition)
  , "Test 2" ~: (Right ('(',"hello world)",(getPosition 1 0))) @=? (runParser (parseOr (parseChar 'f') (parseChar '(')) "(hello world)" defaultPosition)
  , "Test 3" ~: (Right ('(',"hello world)",(getPosition 1 0))) @=? (runParser (parseOr (parseChar '(') (parseChar '(')) "(hello world)" defaultPosition)
  , "Test 4" ~: (Left ("Invalid char found",(getPosition 1 0))) @=? (runParser (parseOr (parseChar 'f') (parseChar 'f')) "(hello world)" defaultPosition)
  ]

-- parseAndWithTests :: Test
-- parseAndWithTests = TestList
--   [ "parseAndWith: "
--   ]

parseAndTests :: Test -- à compléter
parseAndTests = TestList
  [ "Test 1" ~: (Left ("Invalid char found",(getPosition 2 0))) @=? (runParser (parseAnd (parseChar '(') (parseChar 'f')) "(hello world)" defaultPosition)
  ]

parseManyTests :: Test -- à compléter
parseManyTests = TestList
  [ "Test 1" ~: (Right ("      ","hello world!",(getPosition 6 0))) @=? (runParser (parseMany (parseChar ' ')) "      hello world!" defaultPosition)
  ]

parseSomeTests :: Test -- à compléter
parseSomeTests = TestList
  [ "Test 1" ~: (Right ("      ","hello world!",(getPosition 6 0))) @=? (runParser (parseSome (parseChar ' ')) "      hello world!" defaultPosition)
  ]

parseDigitTests :: Test -- à compléter
parseDigitTests = TestList
  [ "Test 1" ~: (Right ('6'," 5 3 7 4 4",(getPosition 1 0))) @=? (runParser parseDigit "6 5 3 7 4 4" defaultPosition)
  ]

parseUIntTests :: Test -- à compléter
parseUIntTests = TestList
  [ "Test 1" ~: (Right (6 ," 5 3 7 4 4",(getPosition 1 0))) @=? (runParser parseUInt "6 5 3 7 4 4" defaultPosition)
  ]

parseNegIntTests :: Test -- à compléter
parseNegIntTests = TestList
  [ "Test 1" ~: (Right (-6, " 5 3 7 4 4",(getPosition 2 0))) @=? (runParser parseNegInt "-6 5 3 7 4 4" defaultPosition)
  ]

parseIntTests :: Test -- à compléter
parseIntTests = TestList
  [ "Test 1" ~: (Right (-6, " 5 3 7 4 4",(getPosition 2 0))) @=? (runParser parseInt "-6 5 3 7 4 4" defaultPosition)
  ]

parseWithSpaceTests :: Test -- à compléter
parseWithSpaceTests = TestList
  [ "Test 1" ~: (Right ('h',"ello world!",(getPosition 6 0))) @=? (runParser (parseWithSpace (parseChar 'h')) "     hello world!" defaultPosition)
  ]

parseOpeningParenthesisTests :: Test -- à compléter
parseOpeningParenthesisTests = TestList
  [ "Test 1" ~: (Right ('(',"hello world!)",(getPosition 1 0))) @=? (runParser parseOpeningParenthesis "(hello world!)" defaultPosition)
  ]

parseClosingParenthesisTests :: Test -- à compléter
parseClosingParenthesisTests = TestList
  [ "Test 1" ~: (Right (')',"",(getPosition 1 0))) @=? (runParser parseClosingParenthesis ")" defaultPosition)
  ]

parsePairTests :: Test -- à compléter
parsePairTests = TestList
  [ "Test 1" ~: (Right (('5','5'),"", (getPosition 4 0))) @=? (runParser (parsePair (parseChar '5')) "(55)" defaultPosition)
  ]

parseListTests :: Test -- à compléter
parseListTests = TestList
  [ "Test 1" ~: (Right ([4,5,6,4,-6],"", getPosition 12 0)) @=? (runParser (parseList parseInt) "(4 5 6 4 -6)" defaultPosition)
  ]
