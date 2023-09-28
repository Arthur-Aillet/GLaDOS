module ParserSpec (parserTests) where

import Test.HUnit
import PositionType (Position(..))
import ParserType
import ParserFunction (parseChar
          , parseAnyChar
          , parseOr
          , parseAnd
          , parseMany
          , parseSome
          , parseDigit
          , parseUInt
          , parseNegInt
          , parseInt
          , parseWithSpace
          , parseOpeningParenthesis
          , parseClosingParenthesis
          , parsePair
          , parseList)

parserTests :: Test
parserTests = TestList
          [ defaultPositionTest
          , moveCursorTests
          , parseCharTests
          , parseAnyCharTests
          , parseOrTests
          , parseAndTests
          , parseManyTests
          , parseSomeTests
          , parseDigitTests
          , parseUIntTests
          , parseNegIntTests
          , parseIntTests
          , parseWithSpaceTests
          , parseOpeningParenthesisTests
          , parseClosingParenthesisTests
          , parsePairTests
          , parseListTests
          ]

getPosition :: Int -> Position
getPosition i | (i == 0) = Position {line = 0 , char = 0}
getPosition i | (i == 1) = Position {line = 10 , char = 4}
getPosition i | (i == 2) = Position {line = 11 , char = 4}
getPosition i | (i == 3) = Position {line = 10 , char = 5}
getPosition i | (i == 4) = Position {line = 1, char = 0}
getPosition i | (i == 5) = Position {line = 0, char = 1}
getPosition i | (i == 6) = Position {line = 0, char = 2}

getParser :: String -> runParser
getParser str | (str == "ParseAnyChar 1") = runParser (parseAnyChar "\nc") "coucou\n" defaultPosition
getParser str | (str == "ParseAnyChar 2") = runParser (parseAnyChar "c\n") "coucou\n" defaultPosition
getParser str | (str == "ParseAnyChar 3") = runParser (parseAnyChar "re" ) "zero\n" defaultPosition

defaultPositionTest :: Test
defaultPositionTest = assertEqual "default position is line = 0, char = 0" (defaultPosition) (getPosition 0)

moveCursorTests :: Test
moveCursorTests = TestList
  [ "moveCursor: Move cursor to the next line" ~: assertEqual "Should return Position with line += 1" (moveCursor (getPosition 1) True) (getPosition 2)
  , "moveCursor: Move cursor to the next char" ~: assertEqual "Should return Position with char += 1" (moveCursor (getPosition 1) True) (getPosition 3)
  ]

parseCharTests :: Test -- à completer
parseCharTests = TestList
  [ "parseChar: parse string with no '\n' at the first position" ~: assertEqual "Should return the string whitout '\n'" (runParser (parseChar '\n') "\nhello world!" defaultPosition) Right ('\n', "coucou", (getPosition 4))
  ]

parseAnyCharTests :: Test -- à completer
parseAnyCharTests = TestList
  [ "parseAnyChar: check if first char is in '\nc'" ~: assertEqual "Should return the string whitout the first char" (getParser "ParseAnyChar 1") (Right ('c',"oucou\n", (getPosition 5)))
  , "parseAnyChar: check if first char is in 'c\n'" ~: assertEqual "Should return the string whitout the first char" (getParser "ParseAnyChar 2") (Right ('c',"oucou\n", (getPosition 5)))
  , "parseAnyChar: check if first char is in 'rem'" ~: assertEqual "Should return 'Invalid char found'" (getParser "ParseAnyChar 3") (Left ("Invalid char found",Position getPosition 5))
  ]

parseOrTests :: Test
parseOrTests = TestList
  [ "parseOr: Test 1" ~: assertEqual "Should return the first result" (runParser (parseOr (parseChar '(') (parseChar 'f')) "(hello world)" defaultPosition) (Right ('(',"hello world)",(getPosition 5)))
  , "parseOr: Test 2" ~: assertEqual "Should return the second result" (runParser (parseOr (parseChar 'f') (parseChar '(')) "(hello world)" defaultPosition) (Right ('(',"hello world)",(getPosition 5)))
  , "parseOr: Test 3" ~: assertEqual "Should return the first result because results are the same" (runParser (parseOr (parseChar '(') (parseChar '(')) "(hello world)" defaultPosition) (Right ('(',"hello world)",(getPosition 5)))
  , "parseOr: Test 4" ~: assertEqual "Should return 'Invalid char found'" (runParser (parseOr (parseChar 'f') (parseChar 'f')) "(hello world)" defaultPosition) (Left ("Invalid char found",(getPosition 5)))
  ]

-- parseAndWithTests :: Test
-- parseAndWithTests = TestList
--   [ "parseAndWith: "
--   ]

parseAndTests :: Test -- à compléter
parseAndTests = TestList
  [ "parseAnd: Test 1" ~: assertEqual "Should return Left" (runParser (parseAnd (parseChar '(') (parseChar 'f')) "(hello world)" defaultPosition) Left ("Invalid char found",(getPosition 6))
  ]

parseManyTests :: Test -- à compléter
parseManyTests = TestList
  [ "parseMany: Test 1" ~: assertEqual "Should return the string without spaces at the start" (runParser (parseMany (parseChar ' ')) "      hello world!    " defaultPosition) (Right ("      ","hello world!    ",(Position {line = 0, char = 6})))
  ]

parseSomeTests :: Test -- à compléter
parseSomeTests = TestList
  [ "parseSome Test 1" ~: assertEqual "Should return the string without spaces at the start" (runParser (parseSome (parseChar ' ')) "      hello world!    " defaultPosition) (Right ("      ","hello world!    ",(Position {line = 0, char = 6})))
  ]

parseDigitTests :: Test -- à compléter
parseDigitTests = TestList
  [ "parseDigit Test 1" ~: assertEqual "Should return" (runParser parseDigit "6 5 3 7 4 4" defaultPosition) (Right ('6'," 5 3 7 4 4",(getPosition 5)))
  ]

parseUIntTests :: Test -- à compléter
parseUIntTests = TestList
  [ "parseUInt Test 1" ~: assertEqual "Should return" (runParser parseDigit "6 5 3 7 4 4" defaultPosition) (Right (6," 5 3 7 4 4",(getPosition 5)))
  ]

parseNegIntTests :: Test -- à compléter
parseNegIntTests = TestList
  [ "parseNegInt Test 1" ~: assertEqual "Should return" (runParser parseNegInt "-6 5 3 7 4 4" defaultPosition) (Right (-6, " 5 3 7 4 4",(getPosition 6)))
  ]

parseIntTests :: Test -- à compléter
parseIntTests = TestList
  [ "parseInt Test 1" ~: assertEqual "Should return" (runParser parseInt "-6 5 3 7 4 4" defaultPosition) (Right (-6, " 5 3 7 4 4",(getPosition 6)))
  ]

parseWithSpaceTests :: Test -- à compléter
parseWithSpaceTests = TestList
  [ "parseWithSpace Test 1" ~: assertEqual "Should return" (runParser (parseWithSpace (parseChar 'h')) "      hello world!    " defaultPosition) (Right ('h',"ello world!    ",(Position {line = 0, char = 7})))
  ]

parseOpeningParenthesisTests :: Test -- à compléter
parseOpeningParenthesisTests = TestList
  [ "parseOpeningParenthesis Test 1" ~: assertEqual "Should return" (runParser parseOpeningParenthesis "(hello world!)" defaultPosition) (Right ('(',"hello world!)",(getPosition 5)))
  ]

parseClosingParenthesisTests :: Test -- à compléter
parseClosingParenthesisTests = TestList
  [ "parseCloseingParenthesis Test 1" ~: assertEqual "Should return" (runParser parseClosingParenthesis ")" defaultPosition) (Right (')',"",(getPosition 5)))
  ]

parsePairTests :: Test -- à compléter
parsePairTests = TestList
  [ "parsePair Test 1" ~: assertEqual "Should return" (runParser (parsePair (parseChar '5')) "(55)" defaultPosition) (Right (('5','5'),"",(Position {line = 0, char = 4})))
  ]

parseListTests :: Test -- à compléter
parseListTests = TestList
  [ "parseList Test 1" ~: assertEqual "Should return" (runParser (parseList parseInt) "(4 5 6 4 -6)" defaultPosition) (Right ([4,5,6,4,-6],"", Position {line = 0, char = 12}))
  ]
