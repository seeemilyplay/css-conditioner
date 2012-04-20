module Test.CSSParserTest where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertFailure, assertEqual, assertBool, Assertion)
import Text.ParserCombinators.Parsec
import Text.Parsec.Error
import Text.Parsec.Token

import CSSParser

main = defaultMain tests

tests =
  [ testCase "numericTerm valid" $ forAllCases assertParsesAs numericTerm
      [ ("50%", NumericTerm 50.0 (Just Percentage))
      , ("2.0cm", NumericTerm 2.0 (Just Centimeter))
      , ("2.0CM", NumericTerm 2.0 (Just Centimeter))
      , ("0.4in", NumericTerm 0.4 (Just Inch))
      , ("0.4IN", NumericTerm 0.4 (Just Inch))
      , ("90deg", NumericTerm 90.0 (Just Degree))
      , ("90DEG", NumericTerm 90.0 (Just Degree))
      , ("2rad", NumericTerm 2.0 (Just Radian))
      , ("2RAD", NumericTerm 2.0 (Just Radian))
      , ("0.3grad", NumericTerm 0.3 (Just Grad))
      , ("0.3GRAD", NumericTerm 0.3 (Just Grad))
      , ("-15s", NumericTerm (-15.0) (Just Second))
      , ("-15S", NumericTerm (-15.0) (Just Second))
      , ("2hz", NumericTerm 2.0 (Just Hertz))
      , ("2HZ", NumericTerm 2.0 (Just Hertz))
      , ("1000khz", NumericTerm 1000 (Just Kilohertz))
      , ("1000KHZ", NumericTerm 1000 (Just Kilohertz))
      , ("+360px", NumericTerm 360.0 (Just Pixel))
      , ("+360PX", NumericTerm 360.0 (Just Pixel))
      , ("12pt", NumericTerm 12.0 (Just Point))
      , ("12PT", NumericTerm 12.0 (Just Point))
      , (".2pc", NumericTerm 0.2 (Just Pica))
      , (".2PC", NumericTerm 0.2 (Just Pica))
      , ("0.01mm", NumericTerm 0.01 (Just Millimeter))
      , ("0.01MM", NumericTerm 0.01 (Just Millimeter))
      , ("500ms", NumericTerm 500 (Just Millisecond))
      , ("500MS", NumericTerm 500 (Just Millisecond))
      , ("0", NumericTerm 0 Nothing)
      ]
  , testCase "numericTerm invalid" $ forAllCases assertCantParseAs numericTerm
      [ ("Gx4", [Expect "numeric term"])
      , ("abc", [Expect "numeric term"])
      ]
  , testCase "stringTerm valid" $ forAllCases assertParsesAs stringTerm
      [ ("''", StringTerm "")
      , ("\"\"", StringTerm "")
      , ("'abc'", StringTerm "abc")
      , ("\"aBc\"", StringTerm "aBc")
      , ("'abc /*blah*/'", StringTerm "abc /*blah*/")
      , ("' a \"b\" c '", StringTerm " a \"b\" c ")
      , ("\" a 'b' c \"", StringTerm " a 'b' c ")
      ]
  , testCase "stringTerm invalid" $ forAllCases assertCantParseAs stringTerm
      [ ("abc", [Expect "quoted string"])
      , ("'abc\rabc'", [])
      , ("\"abc\nabc\"", [])
      ]
  , testCase "identTerm valid" $ forAllCases assertParsesAs identTerm
      [ ("abcd", IdentTerm "abcd")
      , ("asdf/*blah*/", IdentTerm "asdf")
      , ("-JIM", IdentTerm "-jim")
      , ("GRRRR", IdentTerm "grrrr")
      , ("_jim", IdentTerm "_jim")
      , ("-a-a-a", IdentTerm "-a-a-a")
      , ("a234", IdentTerm "a234")
      , ("_bbb", IdentTerm "_bbb")
      , ("_a_2_c", IdentTerm "_a_2_c")
      ]
  , testCase "identTerm invalid" $ forAllCases assertCantParseAs identTerm
      [ ("2bc", [Expect "identifier"])
      , ("#bc", [Expect "identifier"])
      ]
  , testCase "uri valid" $ forAllCases assertParsesAs uri
      [ ("url(\"blah\")", URI("blah"))
      , ("URL('blah')", URI("blah"))
      , ("url(/*asd*/ http:/jim/bob.com/*bbbc*/ )", URI("http:/jim/bob.com"))
      , ("url()", URI(""))
      ]
  , testCase "uri invalid" $ forAllCases assertCantParseAs uri
      [ ("abc", [Expect "uri"])
      , ("url(", [Expect "quoted string", Expect "unquoted string"])
      ]
  , testCase "quotedString valid" $ forAllCases assertParsesAs quotedString
      [ ("''", "")
      , ("\"\"", "")
      , ("'abc'", "abc")
      , ("\"aBc\"", "aBc")
      , ("'abc /*blah*/'", "abc /*blah*/")
      , ("' a \"b\" c '", " a \"b\" c ")
      , ("\" a 'b' c \"", " a 'b' c ")
      ]
  , testCase "quotedString invalid" $ forAllCases assertCantParseAs quotedString
      [ ("abc", [Expect "quoted string"])
      , ("'abc\rabc'", [])
      , ("\"abc\nabc\"", [])
      ]
  , testCase "hexcolor valid" $ forAllCases assertParsesAs hexcolor
      [ ("#fff", RGBColor 255 255 255)
      , ("#FFFFFF", RGBColor 255 255 255)
      , ("#000", RGBColor 0 0 0)
      , ("#000000", RGBColor 0 0 0)
      , ("#5ae", RGBColor 85 170 238)
      , ("#5FAfef", RGBColor 95 175 239)
      , ("#4ab259", RGBColor 74 178 89)
      ]
  , testCase "hexcolor invalid" $ forAllCases assertCantParseAs hexcolor
      [ ("0x56", [Expect "hexcolor"])
      , ("ffffff", [Expect "hexcolor"])
      , ("#abj", [Expect "hexadecimal digit"])
      ]
  , testCase "identifier lexer valid" $ forAllCases assertParsesAs (identifier lexer)
      [ ("abcd", "abcd")
      , ("asdf/*blah*/", "asdf")
      , ("-jim", "-jim")
      , ("GRRRR", "GRRRR")
      , ("_jim", "_jim")
      , ("-a-a-a", "-a-a-a")
      , ("a234", "a234")
      , ("_bbb", "_bbb")
      , ("_a_2_c", "_a_2_c")
      ]
  , testCase "identifier lexer invalid" $ forAllCases assertCantParseAs (identifier lexer)
      [ ("2bc", [Expect "identifier"])
      , ("#bc", [Expect "identifier"])
      ]
  , testCase "num valid" $ forAllCases assertParsesAs num
      [ ("2", 2)
      , ("98", 98)
      , ("103", 103)
      , ("-99", -99)
      , ("+22", 22)
      , ("0.5", 0.5)
      , ("1.77", 1.77)
      , ("23.33", 23.33)
      , (".75", 0.75)
      , ("-82.42", -82.42)
      , ("+89.37", 89.37)
      , ("-.3", -0.3)
      , ("+.42", 0.42)
      ]
  , testCase "num invalid" $ forAllCases assertCantParseAs num
      [ ("a", [Expect "number"])
      , ("- 24", [])
      , (". 2", [])
      ]
  ]

forAllCases asserter p cases = do
  sequence $ map (\(input, expected) -> asserter p input expected) cases
  return ()

assertParsesAs p input expected =
  case parse p input input of
    Left msg -> assertFailure $ "Failed with msg: " ++ show msg
    Right actual -> assertEqual input expected actual

assertCantParseAs p input expected =
  case parse p input input of
    Left err -> assertErrorContains err expected
    Right result -> assertFailure $ "Failed with parse result: " ++ show result

assertErrorContains err [] = return ()
assertErrorContains err (Expect ex:exs) = do
  assertBool ("Should be expecting '" ++ ex ++ "' but got: \n" ++ (show err))
            (ex `elem` (map messageString $ errorMessages err))
  assertErrorContains err exs
