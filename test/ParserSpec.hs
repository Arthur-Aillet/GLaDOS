module ParserSpec (
  parserTests
) where

import Test.HUnit
import PositionType (Position (..))
import ParserType (Parser (..))

parserTests :: Test
parserTests = TestList
          [ defaultPositionTest
          , moveCursorTests
          , parseCharTests
          , parseAnyCharTests
          , parseOrTests
          ]

getPosition :: Int -> Position
getPosition i | (i = 0) = Position {line = 0 , char = 0}
getPosition i | (i = 1) = Position {line = 10 , char = 4}
getPosition i | (i = 2) = Position {line = 11 , char = 4}
getPosition i | (i = 3) = Position {line = 10 , char = 5}
getPosition i | (i = 4) = Position {line = 1, char = 0}
getPosition i | (i = 5) = Position {line = 0, char = 1}

getParser :: Int -> Parser
getParser i | (i = 0) = Parser {runParser :: Right() "runParser", (getPosition 1)}

defaultPositionTest :: Test
defaultPositionTest = ~: assertEqual "default position is line = 0, char = 0" (defaultPosition) (getPosition 0)

moveCursorTests :: Test
moveCursorTests = TestList
  [ "moveCursor: Move cursor to the next line" ~: assertEqual "Should return Position with line += 1" (moveCursor (getPosition 1) True) (getPosition 2)
  , "moveCursor: Move cursor to the next char" ~: assertEqual "Should return Position with char += 1" (moveCursor (getPosition 1) True) (getPosition 3)
  ]

parseCharTests :: Test -- à completer
parseCharTests = TestList
  [ "parseChar: parse string with no '\n' at the first position"
    ~: assertEqual "Should return the string whitout '\n'" (runParser (parseChar '\n') "\nhello world!" defaultPosition) Right ('\n', "coucou", (getPosition 4))
  ]

parseAnyCharTests :: Test -- à completer
parseAnyCharTests = TestList
  [ "parseAnyChar: check if first char is in '\nc'" ~: assertEqual "Should return the string whitout the first char" (runParser (parseAnyChar "\nc") "coucou\n" defaultPosition) (Right ('c',"oucou\n",Position {line = 0, char = 1}))
  , "parseAnyChar: check if first char is in 'c\n'" ~: assertEqual "Should return the string whitout the first char" (runParser (parseAnyChar "c\n") "coucou\n" defaultPosition) (Right ('c',"oucou\n",Position {line = 0, char = 1}))
  , "parseAnyChar: check if first char is in 'rem'" ~: assertEqual "Should return 'Invalid char found'" (runParser (parseAnyChar "re" ) "zero\n" defaultPosition) (Left ("Invalid char found",Position {line = 0, char = 1}))
  ]

parseOrTests :: Test
parseOrTests = TestList
  [ "parseOr: Test 1" ~: assertEqual "Should return the first result" (runParser (parseOr (parseChar '(') (parseChar 'f')) "(hello world)" defaultPosition) (Right ('(',"hello world)",Position {line = 0, char = 1}))
  , "parseOr: Test 2" ~: assertEqual "Should return the second result" (runParser (parseOr (parseChar 'f') (parseChar '(')) "(hello world)" defaultPosition) (Right ('(',"hello world)",Position {line = 0, char = 1}))
  , "parseOr: Test 3" ~: assertEqual "Should return the first result because results are the same" (runParser (parseOr (parseChar '(') (parseChar '(')) "(hello world)" defaultPosition) (Right ('(',"hello world)",Position {line = 0, char = 1}))
  , "parseOr: Test 4" ~: assertEqual "Should return 'Invalid char found'" (runParser (parseOr (parseChar 'f') (parseChar 'f')) "(hello world)" defaultPosition) (Left ("Invalid char found",Position {line = 0, char = 1}))
  ]

-- parseAndWithTests :: Test
-- parseAndWithTests = TestList
--   [ "parseAndWith: "
--   ]