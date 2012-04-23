module CSSParserTest where

import Test.Framework (defaultMain, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertFailure, assertEqual, assertBool, Assertion)
import Text.Parsec
import Text.Parsec.Error
import Text.Parsec.Token


import CSSParser

runTests :: IO ()
runTests = defaultMain tests

tests :: [Test]
tests =
  [ testCase "stylesheet valid" $ forAllCases assertParsesAs stylesheet
      [ ("", Stylesheet Nothing [])
      , ("/*hey lovelies*/ a {} b {}",
         Stylesheet Nothing
                    [ RuleSet [Selector [SimpleSelector [NamedElement "a"]]] []
                    , RuleSet [Selector [SimpleSelector [NamedElement "b"]]] []
                    ])
      , ("@charset 'utf-8'; a {} b {}",
         Stylesheet (Just $ CharSet "utf-8")
                    [ RuleSet [Selector [SimpleSelector [NamedElement "a"]]] []
                    , RuleSet [Selector [SimpleSelector [NamedElement "b"]]] []
                    ])
      ]
  , testCase "stylesheet invalid" $ forAllCases assertCantParseAs stylesheet
      [ ("a", [])
      , ("@", [])
      ]
  , testCase "charset valid" $ forAllCases assertParsesAs charset
      [ ("@charset \"UTF-8\";", CharSet "utf-8")
      , ("@charset 'utf-8';", CharSet "utf-8")
      , ("@CHARSET    \"utf-8\";", CharSet "utf-8")
      ]
  , testCase "charset invalid" $ forAllCases assertCantParseAs charset
      [ ("@charset utf-9;", [])
      , ("charset 'utf-8';", [])
      , ("@charset 'utf-8'", [])
      ]
  , testCase "media valid" $ forAllCases assertParsesAs media
      [ ("@MEDIA SCREEN, PRINT { p.test {font-weight:bold;} }",
          Media ["screen", "print"]
                [RuleSet [Selector [SimpleSelector [NamedElement "p", Class "test"]]]
                         [Declaration (Property "font-weight" Nothing)
                                      [IdentTerm "bold"]
                                      Nothing]
                ])
      , ("@media screen { }",
          Media ["screen"]
                [])
      ]
  , testCase "media invalid" $ forAllCases assertCantParseAs media
      [ ("@media screen", [])
      , ("media screen", [Expect "@media"])
      ]
  , testCase "ruleset valid" $ forAllCases assertParsesAs ruleset
      [ (".account-body h2, .tab-viewer .account-body h2 " ++
         "{ color: #000; font: 14px/24px \"Helvetica Neue\";}",
         RuleSet [ Selector [ SimpleSelector [Class "account-body"]
                            , SimpleSelector [NamedElement "h2"]
                            ]
                 , Selector [ SimpleSelector [Class "tab-viewer"]
                            , SimpleSelector [Class "account-body"]
                            , SimpleSelector [NamedElement "h2"]
                            ]
                 ]
                 [ Declaration (Property "color" Nothing)
                               [ RGBColor 0 0 0 ]
                               Nothing
                 , Declaration (Property "font" Nothing)
                               [ NumericTerm 14.0 (Just Pixel)
                               , NumericTerm 24.0 (Just Pixel)
                               , StringTerm "Helvetica Neue"]
                               Nothing
                 ])
      ]
  , testCase "ruleset invalid" $ forAllCases assertCantParseAs selectors
      [ ("", [Expect "selector"])
      , ("{blah: 0px;}, ", [Expect "selector"])
      , (".class,{}", [Expect "selector"])
      ]
  , testCase "selectors valid" $ forAllCases assertParsesAs selectors
      [ ("html, body,div",
         [ Selector [ SimpleSelector [NamedElement "html"] ]
         , Selector [ SimpleSelector [NamedElement "body"] ]
         , Selector [ SimpleSelector [NamedElement "div"] ]
         ])
      , (".login-container .body, *.Txt-Link TD.body:hover, a.txt-link",
         [ Selector [ SimpleSelector [Class "login-container"]
                    , SimpleSelector [Class "body"] ]
         , Selector [ SimpleSelector [WildcardElement, Class "Txt-Link"]
                    , SimpleSelector [NamedElement "td", Class "body", Pseudo "hover"] ]
         , Selector [ SimpleSelector [NamedElement "a", Class "txt-link"] ]
         ])
      ]
  , testCase "selectors invalid" $ forAllCases assertCantParseAs selectors
      [ ("", [Expect "selector"])
      , ("html, ", [Expect "selector"])
      , ("html, {", [Expect "selector"])
      ]
  , testCase "selector invalid" $ forAllCases assertCantParseAs selector
      [ ("!", [Expect "selector"])
      , ("td.class td.body:", [Expect "identifier"])
      ]
  , testCase "selector valid" $ forAllCases assertParsesAs selector
      [ ("#top-container .intro-container p.introtext",
        Selector [ SimpleSelector [Id "top-container"]
                 , SimpleSelector [Class "intro-container"]
                 , SimpleSelector [NamedElement "p", Class "introtext"]
                 ])
      , (".login-container .body *.Txt-Link > TD.body:hover a.txt-link",
        Selector [ SimpleSelector [Class "login-container"]
                 , SimpleSelector [Class "body"]
                 , SimpleSelector [WildcardElement, Class "Txt-Link"]
                 , NearestChildSelector [NamedElement "td", Class "body", Pseudo "hover"]
                 , SimpleSelector [NamedElement "a", Class "txt-link"]
                 ])
      ]
  , testCase "selector invalid" $ forAllCases assertCantParseAs selector
      [ ("!", [Expect "selector"])
      , ("td.class td.body:", [Expect "identifier"])
      ]
  , testCase "nearestChildSelector valid" $ forAllCases assertParsesAs nearestChildSelector
      [ ("> a:hover",
         NearestChildSelector [NamedElement "a", Pseudo "hover"])
      , (">.squeeze.lime",
         NearestChildSelector [Class "squeeze", Class "lime"])
      ]
  , testCase "nearestChildSelector invalid" $ forAllCases assertCantParseAs nearestChildSelector
      [ ("a:hover", [])
      , (".squeeze.lime", [])
      , ("#footer", [])
      ]
  , testCase "plusSelector valid" $ forAllCases assertParsesAs plusSelector
      [ ("+ a:hover",
         PlusSelector [NamedElement "a", Pseudo "hover"])
      , ("+.squeeze.lime",
         PlusSelector [Class "squeeze", Class "lime"])
      ]
  , testCase "plusSelector invalid" $ forAllCases assertCantParseAs plusSelector
      [ ("a:hover", [])
      , (".squeeze.lime", [])
      , ("#footer", [])
      ]
  , testCase "simpleSelector valid" $ forAllCases assertParsesAs simpleSelector
      [ ("TD.CLASS#ID", SimpleSelector [NamedElement "td", Class "CLASS", Id "ID"])
      , (".CLASS-1#ID-1.class-2#id-2",
         SimpleSelector [Class "CLASS-1", Id "ID-1", Class "class-2", Id "id-2"])
      , ("a:HOVER", SimpleSelector [NamedElement "a", Pseudo "hover"])
      , (".happy", SimpleSelector [Class "happy"])
      ]
  , testCase "simpleSelector invalid" $ forAllCases assertCantParseAs simpleSelector
      [ ("!", [Expect "#id", Expect ".class", Expect "element-name", Expect ":pseudo-element"])
      , ("td.class.9", [Expect "identifier"])
      ]
  , testCase "selectorTerms valid" $ forAllCases assertParsesAs selectorTerms
      [ ("TD.CLASS#ID", [NamedElement "td", Class "CLASS", Id "ID"])
      , (".CLASS-1#ID-1.class-2#id-2",
         [Class "CLASS-1", Id "ID-1", Class "class-2", Id "id-2"])
      , ("a:HOVER", [NamedElement "a", Pseudo "hover"])
      , (".happy", [Class "happy"])
      , ("input[type='text'].tooltip",
         [ NamedElement "input"
         , Attribute "type" (Just $ EqualMatch "text")
         , Class "tooltip" ])
      ]
  , testCase "selectorTerms invalid" $ forAllCases assertCantParseAs selectorTerms
      [ ("!", [Expect "#id", Expect ".class", Expect "element-name", Expect ":pseudo-element"])
      , ("td.class.9", [Expect "identifier"])
      ]
  , testCase "elementName valid" $ forAllCases assertParsesAs elementName
      [ ("TD", NamedElement "td")
      , ("*", WildcardElement)
      , ("p", NamedElement "p")
      ]
  , testCase "elementName invalid" $ forAllCases assertCantParseAs elementName
      [ ("!importnat", [Expect "element-name"])
      , (".class", [Expect "element-name"])
      , (":pseudo", [Expect "element-name"])
      , ("#$22", [Expect "element-name"])
      ]
  , testCase "attribute valid" $ forAllCases assertParsesAs attribute
      [ ("[selected]", Attribute "selected" Nothing)
      , ("[SELECTED]", Attribute "SELECTED" Nothing)
      , ("[type='INPUT']", Attribute "type" (Just $ EqualMatch "INPUT"))
      , ("[LANG~='en, DE']", Attribute "LANG" (Just $ IncludesMatch "en, DE"))
      , ("[lang|='En']", Attribute "lang" (Just $ DashMatch "En"))
      ]
  , testCase "id valid" $ forAllCases assertParsesAs identity
      [ ("#Keep_Case", Id "Keep_Case")
      , ("#_a-b-C", Id "_a-b-C")
      , ("#-a_b-C", Id "-a_b-C")
      , ("#2222", Id "2222")
      ]
  , testCase "id invalid" $ forAllCases assertCantParseAs identity
      [ ("not-hash", [Expect "#id"])
      , (".class", [Expect "#id"])
      , (":pseudo", [Expect "#id"])
      , ("#$22", [])
      ]
  , testCase "class valid" $ forAllCases assertParsesAs clazz
      [ (".Keep_Case", Class "Keep_Case")
      , ("._a-b-C", Class "_a-b-C")
      , (".-a_b-C", Class "-a_b-C")
      ]
  , testCase "class invalid" $ forAllCases assertCantParseAs clazz
      [ ("not-dot", [Expect ".class"])
      , ("#id", [Expect ".class"])
      , (":pseudo", [Expect ".class"])
      ]
  , testCase "pseudoElement valid" $ forAllCases assertParsesAs pseudoElement
      [ (":HOVER", Pseudo "hover")
      , (":last-child", Pseudo "last-child")
      ]
  , testCase "pseudoElement invalid" $ forAllCases assertCantParseAs pseudoElement
      [ (".hover", [Expect ":pseudo-element"])
      , ("#id", [Expect ":pseudo-element"])
      ]
  , testCase "declarations valid" $ forAllCases assertParsesAs declarations
      [ ("", [])
      , (";", [])
      , ("; border:",
         [ Declaration (Property "border" Nothing) [] Nothing])
      , ("border:",
         [ Declaration (Property "border" Nothing) [] Nothing])
      , ("border: 1px #fff;",
         [ Declaration (Property "border" Nothing)
                       [NumericTerm 1.0 (Just Pixel), RGBColor 255 255 255]
                       Nothing])
      , ("width: 20; height: 30",
         [ Declaration (Property "width" Nothing)
                       [NumericTerm 20 Nothing]
                       Nothing
         , Declaration (Property "height" Nothing)
                       [NumericTerm 30 Nothing]
                       Nothing])
      ]
  , testCase "declarations invalid" $ forAllCases assertCantParseAs declarations
      [ ("jim: bob; border 1px solid #9e9e9e", [])
      ]
  , testCase "declaration valid" $ forAllCases assertParsesAs declaration
      [ ("MARGIN:/*hey*/ 0 !IMPORTANT",
         Declaration (Property "margin" Nothing)
                     [ NumericTerm 0 Nothing ]
                     (Just $ Priority "important")),
        ("*font: 12px \"Helvetica Neue\"",
         Declaration (Property "font" (Just $ PropertyHack '*'))
                     [ NumericTerm 12.0 (Just Pixel),
                       StringTerm "Helvetica Neue" ]
                     Nothing),
        ("background: url(../img/btn-small.png) 0 0 no-repeat",
         Declaration (Property "background" Nothing)
                     [ URI "../img/btn-small.png",
                       NumericTerm 0 Nothing,
                       NumericTerm 0 Nothing,
                       IdentTerm "no-repeat" ]
                     Nothing),
        ("-moz-border-radius: 5PX 5PX 5PX 5PX",
         Declaration (Property "-moz-border-radius" Nothing)
                     [ NumericTerm 5.0 (Just Pixel),
                       NumericTerm 5.0 (Just Pixel),
                       NumericTerm 5.0 (Just Pixel),
                       NumericTerm 5.0 (Just Pixel) ]
                     Nothing),
        ("border: 1px solid #9e9e9e",
         Declaration (Property "border" Nothing)
                     [ NumericTerm 1.0 (Just Pixel),
                       IdentTerm "solid",
                       RGBColor 158 158 158 ]
                     Nothing),
        ("border: ",
         Declaration (Property "border" Nothing)
                     []
                     Nothing),
        ("border: !important",
         Declaration (Property "border" Nothing)
                     []
                     (Just $ Priority "important"))
      ]
  , testCase "declaration invalid" $ forAllCases assertCantParseAs declaration
      [ (";", [Expect "declaration"])
      , ("2border: 1px solid #9e9e9e", [Expect "declaration"])
      , ("border 1px solid #9e9e9e", [])
      ]
  , testCase "property valid" $ forAllCases assertParsesAs property
      [ ("_abc", Property "abc" (Just $ PropertyHack '_'))
      , ("ABCCC", Property "abccc" Nothing)
      , ("-moz-border-radius", Property "-moz-border-radius" Nothing)
      ]
  , testCase "property invalid" $ forAllCases assertCantParseAs property
      [ ("!important", [])
      , (";", [])
      , (":", [])
      , ("124", [])
      ]
  , testCase "hackedProperty valid" $ forAllCases assertParsesAs hackedProperty
      [ ("*abc", Property "abc" (Just $ PropertyHack '*'))
      , ("_ABC", Property "abc" (Just $ PropertyHack '_'))
      , ("#A-c", Property "a-c" (Just $ PropertyHack '#'))
      ]
  , testCase "hackedProperty invalid" $ forAllCases assertCantParseAs hackedProperty
      [ ("-moz-border-radius", [])
      , ("!important", [])
      , (";", [])
      , (":", [])
      ]
  , testCase "unhackedProperty valid" $ forAllCases assertParsesAs unhackedProperty
      [ ("abc", Property "abc" Nothing)
      , ("abc-CDE", Property "abc-cde" Nothing)
      ]
  , testCase "unhackedProperty invalid" $ forAllCases assertCantParseAs unhackedProperty
      [ (";", [Expect "identifier"])
      , ("*blah", [Expect "identifier"])
      ]
  , testCase "priority valid" $ forAllCases assertParsesAs priority
      [ ("!important", Priority "important")
      , ("!FRED", Priority "fred")
      , ("!/*blah*/important", Priority "important")
      ]
  , testCase "priority invalid" $ forAllCases assertCantParseAs priority
      [ ("important", [Expect "priority"])
      ]
  , testCase "expr valid" $ forAllCases assertParsesAs expr
      [ ("", [])
      , ("#000", [RGBColor 0 0 0])
      , ("bold ITALIC small-caps 1em/1.5em \"Verdana\",Sans-Serif",
         [ IdentTerm "bold"
         , IdentTerm "italic"
         , IdentTerm "small-caps"
         , NumericTerm 1.0 (Just Ems)
         , NumericTerm 1.5 (Just Ems)
         , StringTerm "Verdana"
         , IdentTerm "sans-serif" ])
      , ("URL(blah.com) /*hey*/ 20% 20 20px NO-repeat urllll",
         [ URI "blah.com"
         , NumericTerm 20.0 (Just Percentage)
         , NumericTerm 20.0 Nothing
         , NumericTerm 20.0 (Just Pixel)
         , IdentTerm "no-repeat"
         , IdentTerm "urllll" ])
      , ("#ffffff /*hey ho*/      \"BLAH\"",
         [ RGBColor 255 255 255
         , StringTerm "BLAH" ])
      , ("alpha(opacity=1) url(a.b) f(a) asd",
         [ OpacityHack 1.0
         , URI "a.b"
         , FunctionTerm "f" [ IdentTerm "a" ]
         , IdentTerm "asd" ])
      ]
  , testCase "op valid" $ forAllCases assertParsesAs termSeparator
      [ ("/", "/")
      , (",", ",")
      ]
  , testCase "op invalid" $ forAllCases assertCantParseAs termSeparator
      [ ("a", [Expect "term separator"])
      , ("abc", [Expect "term separator"])
      ]
  , testCase "numericTerm valid" $ forAllCases assertParsesAs numericTerm
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
      , ("0.9999em", NumericTerm 0.9999 (Just Ems))
      , ("0.9999em", NumericTerm 0.9999 (Just Ems))
      , ("-.22ex", NumericTerm (-0.22) (Just Exs))
      , ("-.22ex", NumericTerm (-0.22) (Just Exs))
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
      [ ("url(\"blah\")", URI "blah")
      , ("URL('blah')", URI "blah")
      , ("url(/*asd*/ http:/jim/bob.com/*bbbc*/ )", URI "http:/jim/bob.com")
      , ("url()", URI "")
      ]
  , testCase "uri invalid" $ forAllCases assertCantParseAs uri
      [ ("abc", [Expect "uri"])
      , ("url(", [Expect "quoted string", Expect "unquoted string"])
      ]
  , testCase "opacityHack valid" $ forAllCases assertParsesAs opacityHack
      [ ("alpha(opacity=0.5)", OpacityHack 0.5)
      , ("ALPHA(/*hey*/OPACITY = .2)", OpacityHack 0.2)
      ]
  , testCase "opacityHack invalid" $ forAllCases assertCantParseAs opacityHack
      [ ("alpha(0.5)", [])
      , ("alpha(opacity)", [])
      , ("alpha('opacity'=1)", [])
      , ("opacity=2", [])
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
      , ("#ffff 12px", [])
      , ("#fffffff 12px", [])
      ]
  , testCase "unlexemedIdentifier valid" $ forAllCases assertParsesAs unlexemedIdentifier
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
  , testCase "unlexemedIdentifier invalid" $ forAllCases assertCantParseAs unlexemedIdentifier
      [ ("2bc", [Expect "identifier"])
      , ("#bc", [Expect "identifier"])
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
  , testCase "function valid" $ forAllCases assertParsesAs functionTerm
      [ ("gah()", FunctionTerm "gah" [])
      , ("func(A, B, c)",
         FunctionTerm "func"
          [ IdentTerm "a"
          , IdentTerm "b"
          , IdentTerm "c"])
      , ("AAAA(2.0PX 'Blah' 3.0%)",
         FunctionTerm "aaaa"
           [ NumericTerm 2.0 (Just Pixel)
           , StringTerm "Blah"
           , NumericTerm 3.0 (Just Percentage) ])
      ]
  , testCase "function invalid" $ forAllCases assertCantParseAs functionTerm
      [ ("1ab(3)", [Expect "function"])
      , ("()", [Expect "function"])
      , ("a", [])
      , ("a(2", [])
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

forAllCases :: (Show a, Eq a) => (Parsec String () a -> String -> b -> Assertion)
                              -> Parsec String () a
                              -> [(String, b)]
                              -> Assertion
forAllCases asserter p = mapM_ (uncurry (asserter p))

assertParsesAs :: (Show a, Eq a) => Parsec String () a
                                 -> String
                                 -> a
                                 -> Assertion
assertParsesAs p input expected =
  case parse p input input of
    Left msg -> assertFailure $ "Failed with msg: " ++ show msg
    Right actual -> assertEqual input expected actual

assertCantParseAs :: (Show a, Eq a) => Parsec String () a
                                    -> String
                                    -> [Message]
                                    -> Assertion
assertCantParseAs p input expected =
  case parse p input input of
    Left err -> assertErrorContains err expected
    Right result -> assertFailure $ "Failed with parse result: " ++ show result

assertErrorContains :: ParseError -> [Message] -> Assertion
assertErrorContains _err [] = return ()
assertErrorContains err (Expect e:es) = do
  assertBool ("Should be expecting '" ++ e ++ "' but got: \n" ++ show err)
            (e `elem` map messageString (errorMessages err))
  assertErrorContains err es
assertErrorContains err (_:es) = assertErrorContains err es
