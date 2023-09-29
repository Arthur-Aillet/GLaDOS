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
          , "parseNegIntm" ~: parseNegIntTests
          -- , parseIntTests
          -- , parseWithSpaceTests
          -- , parseOpeningParenthesisTests
          -- , parseClosingParenthesisTests
          -- , parsePairTests
          -- , parseListTests
          ]

getPosition :: Int -> Position
getPosition 0 = Position {line = 0, char = 0}
getPosition 1 = Position {line = 1, char = 0}
getPosition 2 = Position {line = 0, char = 1}
getPosition 3 = Position {line = 0, char = 2}
getPosition 6 = Position {line = 0, char = 6}
getPosition _ = defaultPosition

defaultPositionTest :: Test
defaultPositionTest = TestCase $ assertEqual "default position is line = 0, char = 0" (getPosition 0) defaultPosition

moveCursorTests :: Test
moveCursorTests = TestList
  [ "Move cursor to the next char" ~: (getPosition 1) @=? (moveCursor (getPosition 0) True)
  , "Move cursor to the next line" ~: (getPosition 2) @=? (moveCursor (getPosition 0) False)
  ]

parseCharTests :: Test -- à completer
parseCharTests = TestList
  [ "delete the first character '\n'" ~: (Right ('\n', "hello world!", (getPosition 1))) @=? (runParser (parseChar '\n') "\nhello world!" defaultPosition)
  ]

parseAnyCharTests :: Test -- à completer
parseAnyCharTests = TestList
  [ "check if first char is in '\nc'" ~: (Right ('c',"oucou\n", (getPosition 2))) @=? (runParser (parseAnyChar "\nc") "coucou\n" defaultPosition)
  , "check if first char is in 'c\n'" ~: (Right ('c',"oucou\n", (getPosition 2))) @=? (runParser (parseAnyChar "c\n") "coucou\n" defaultPosition)
  , "check if first char is in 'rem'" ~: (Left ("Char not found in list", (getPosition 0))) @=? (runParser (parseAnyChar "re" ) "zero\n" defaultPosition)
  ]

parseOrTests :: Test
parseOrTests = TestList
  [ "Test 1" ~: (Right ('(',"hello world)",(getPosition 2))) @=? (runParser (parseOr (parseChar '(') (parseChar 'f')) "(hello world)" defaultPosition)
  , "Test 2" ~: (Right ('(',"hello world)",(getPosition 2))) @=? (runParser (parseOr (parseChar 'f') (parseChar '(')) "(hello world)" defaultPosition)
  , "Test 3" ~: (Right ('(',"hello world)",(getPosition 2))) @=? (runParser (parseOr (parseChar '(') (parseChar '(')) "(hello world)" defaultPosition)
  , "Test 4" ~: (Left ("Invalid char found",(getPosition 2))) @=? (runParser (parseOr (parseChar 'f') (parseChar 'f')) "(hello world)" defaultPosition)
  ]

-- parseAndWithTests :: Test
-- parseAndWithTests = TestList
--   [ "parseAndWith: "
--   ]

parseAndTests :: Test -- à compléter
parseAndTests = TestList
  [ "Test 1" ~: (Left ("Invalid char found",(getPosition 3))) @=? (runParser (parseAnd (parseChar '(') (parseChar 'f')) "(hello world)" defaultPosition)
  ]

parseManyTests :: Test -- à compléter
parseManyTests = TestList
  [ "Test 1" ~: (Right ("      ","hello world!",(getPosition 6))) @=? (runParser (parseMany (parseChar ' ')) "      hello world!" defaultPosition)
  ]

parseSomeTests :: Test -- à compléter
parseSomeTests = TestList
  [ "Test 1" ~: (Right ("      ","hello world!",(getPosition 6))) @=? (runParser (parseSome (parseChar ' ')) "      hello world!" defaultPosition)
  ]

parseDigitTests :: Test -- à compléter
parseDigitTests = TestList
  [ "Test 1" ~: (Right ('6'," 5 3 7 4 4",(getPosition 2))) @=? (runParser parseDigit "6 5 3 7 4 4" defaultPosition)
  ]

parseUIntTests :: Test -- à compléter
parseUIntTests = TestList
  [ "Test 1" ~: (Right (6 ," 5 3 7 4 4",(getPosition 2))) @=? (runParser parseUInt "6 5 3 7 4 4" defaultPosition)
  ]

parseNegIntTests :: Test -- à compléter
parseNegIntTests = TestList
  [ "Test 1" ~: (Right (-6, " 5 3 7 4 4",(getPosition 3))) @=? (runParser parseNegInt "-6 5 3 7 4 4" defaultPosition)
  ]

-- parseIntTests :: Test -- à compléter
-- parseIntTests = TestList
--   [ "parseInt Test 1" ~: assertEqual "Should return" (runParser parseInt "-6 5 3 7 4 4" defaultPosition) (Right (-6, " 5 3 7 4 4",(getPosition 6)))
--   ]

-- parseWithSpaceTests :: Test -- à compléter
-- parseWithSpaceTests = TestList
--   [ "parseWithSpace Test 1" ~: assertEqual "Should return" (runParser (parseWithSpace (parseChar 'h')) "      hello world!    " defaultPosition) (Right ('h',"ello world!    ",(Position {line = 0, char = 7})))
--   ]

-- parseOpeningParenthesisTests :: Test -- à compléter
-- parseOpeningParenthesisTests = TestList
--   [ "parseOpeningParenthesis Test 1" ~: assertEqual "Should return" (runParser parseOpeningParenthesis "(hello world!)" defaultPosition) (Right ('(',"hello world!)",(getPosition 5)))
--   ]

-- parseClosingParenthesisTests :: Test -- à compléter
-- parseClosingParenthesisTests = TestList
--   [ "parseCloseingParenthesis Test 1" ~: assertEqual "Should return" (runParser parseClosingParenthesis ")" defaultPosition) (Right (')',"",(getPosition 5)))
--   ]

-- parsePairTests :: Test -- à compléter
-- parsePairTests = TestList
--   [ "parsePair Test 1" ~: assertEqual "Should return" (runParser (parsePair (parseChar '5')) "(55)" defaultPosition) (Right (('5','5'),"",(Position {line = 0, char = 4})))
--   ]

-- parseListTests :: Test -- à compléter
-- parseListTests = TestList
--   [ "parseList Test 1" ~: assertEqual "Should return" (runParser (parseList parseInt) "(4 5 6 4 -6)" defaultPosition) (Right ([4,5,6,4,-6],"", Position {line = 0, char = 12}))
--   ]
