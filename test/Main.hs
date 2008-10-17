module Main where

import Text.PrettyPrint.NestedHughesPJ hiding (Doc)

data Exp = App Exp [Exp]
         | Var String
         | Lit Int
         | List [Exp]


pretty :: Exp -> Doc
pretty (App e args) = pretty e <+> sep (map (parens . pretty) args)
pretty (Var v)      = text v
pretty (Lit l)      = group BOLD $ group GREEN $ text (show l)
pretty (List es)    = brackets (group RED (sep (punctuate (text ",") (map pretty es))))

type Doc = MDoc Markup

data Markup = Open MStyle | Close MStyle

data MStyle = BOLD | RED | GREEN

open :: MStyle -> Doc
open = mark . Open

close :: MStyle -> Doc
close = mark . Close

group :: MStyle -> Doc -> Doc
group s d = open s <> d <> close s

example1 = Var "abc"
example2 = Var "cde"
example3 = App example1 [example2]
example4 = Lit 99
example5 = App example3 [example1,example4,example2,example1,example2,example4,example2]
example6 = List [example1,example2,example3,example4,example5]
example7 = App example3 [example6]
example8 = App example4 [example4]

examples = [ example1
           , example2
           , example3
           , example4
           , example5
           , example6
           , example7
           , example8
           ]

main = do
  putStrLn "<html><body><pre>"
  sequence_ [ putStrLn $ render (pretty ex)
             |  ex <- examples
            ]
  putStrLn "</pre><hr><pre>"
  sequence_ [ putStrLn $ fullRender PageMode 100 1.5 html_txt "" (pretty ex) 
             | ex <- examples
            ]
  putStrLn "</pre></body></html>"

html_txt (Chr c) r      = concatMap escape [c] ++ r
html_txt (Str s) r      = concatMap escape s ++ r
html_txt (PStr s) r     = concatMap escape s ++ r
html_txt (Mark (Open BOLD)) r  = "<b>" ++ r 
html_txt (Mark (Close BOLD)) r = "</b>" ++ r
html_txt (Mark (Open RED)) r   = "<font color=\"red\">" ++ r 
html_txt (Mark (Close RED)) r  = "</font>" ++ r
html_txt (Mark (Open GREEN)) r   = "<font color=\"green\">" ++ r 
html_txt (Mark (Close GREEN)) r  = "</font>" ++ r

escape '<' = "&lt;"
escape '>' = "&gt;"
escape c   = [c]

