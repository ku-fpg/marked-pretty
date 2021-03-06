{-# LANGUAGE BangPatterns, CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveGeneric #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.MarkedHughesPJ
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Andy Gill <andygill@ku.edu>
-- Stability   :  stable
-- Portability :  portable
--
-- Provides a collection of pretty printer combinators, a set of API's
-- that provides a way to easily print out text in a consistent format
-- of your choosing.
--
-- Originally designed by John Hughes's and Simon Peyton Jones's.
-- 
-- Marking added by Andy Gill, Oct 08.
-- 
-- For more information you can refer to the
-- <http://belle.sourceforge.net/doc/hughes95design.pdf original paper> that
-- serves as the basis for this libraries design:
-- /The Design -- of a Pretty-printing Library/ by John Hughes, in Advanced
-- Functional Programming, 1995
-- 
-----------------------------------------------------------------------------

module Text.PrettyPrint.MarkedHughesPJ (

        -- * The document type
        Doc,  -- Type synonym
        MDoc, -- Abstract
        TextDetails(..),

        -- * Constructing documents

        -- ** Converting values into documents
        char, text, ptext, sizedText, zeroWidthText,
        int, integer, float, double, rational,

        -- ** Simple derived documents
        semi, comma, colon, space, equals,
        lparen, rparen, lbrack, rbrack, lbrace, rbrace,

        -- ** Wrapping documents in delimiters
        parens, brackets, braces, quotes, doubleQuotes,
        maybeParens, maybeBrackets, maybeBraces, maybeQuotes, maybeDoubleQuotes,

        -- ** Combining documents
        empty,
        (<>), (<+>), hcat, hsep,
        ($$), ($+$), vcat,
        sep, cat,
        fsep, fcat,
        nest,
        hang, punctuate,

        -- * Predicates on documents
        isEmpty,

        -- * Utility functions for documents
        first, reduceDoc,

        -- * Rendering documents

        -- ** Default rendering
        render,

        -- ** Rendering with a particular style
        Style(..),
        style,
        renderStyle,
        Mode(..),

        -- ** General rendering
        fullRender,

        -- ** Markup extension
        mark
    ) where

import Control.DeepSeq ( NFData(rnf) )
import Data.Function   ( on )
#if __GLASGOW_HASKELL__ < 709
import Data.Monoid     ( Monoid(mempty, mappend)  )
#endif
import Data.String     ( IsString(fromString) )

import GHC.Generics

-- ---------------------------------------------------------------------------
-- The MDoc calculus

{-
Laws for $$
~~~~~~~~~~~
<a1>    (x $$ y) $$ z   = x $$ (y $$ z)
<a2>    empty $$ x      = x
<a3>    x $$ empty      = x

        ...ditto $+$...

Laws for <>
~~~~~~~~~~~
<b1>    (x <> y) <> z   = x <> (y <> z)
<b2>    empty <> x      = empty
<b3>    x <> empty      = x

        ...ditto <+>...

Laws for text
~~~~~~~~~~~~~
<t1>    text s <> text t        = text (s++t)
<t2>    text "" <> x            = x, if x non-empty

** because of law n6, t2 only holds if x doesn't
** start with `nest'.


Laws for nest
~~~~~~~~~~~~~
<n1>    nest 0 x                = x
<n2>    nest k (nest k' x)      = nest (k+k') x
<n3>    nest k (x <> y)         = nest k x <> nest k y
<n4>    nest k (x $$ y)         = nest k x $$ nest k y
<n5>    nest k empty            = empty
<n6>    x <> nest k y           = x <> y, if x non-empty

** Note the side condition on <n6>!  It is this that
** makes it OK for empty to be a left unit for <>.

Miscellaneous
~~~~~~~~~~~~~
<m1>    (text s <> x) $$ y = text s <> ((text "" <> x) $$
                                         nest (-length s) y)

<m2>    (x $$ y) <> z = x $$ (y <> z)
        if y non-empty


Laws for list versions
~~~~~~~~~~~~~~~~~~~~~~
<l1>    sep (ps++[empty]++qs)   = sep (ps ++ qs)
        ...ditto hsep, hcat, vcat, fill...

<l2>    nest k (sep ps) = sep (map (nest k) ps)
        ...ditto hsep, hcat, vcat, fill...

Laws for oneLiner
~~~~~~~~~~~~~~~~~
<o1>    oneLiner (nest k p) = nest k (oneLiner p)
<o2>    oneLiner (x <> y)   = oneLiner x <> oneLiner y

You might think that the following verion of <m1> would
be neater:

<3 NO>  (text s <> x) $$ y = text s <> ((empty <> x)) $$
                                         nest (-length s) y)

But it doesn't work, for if x=empty, we would have

        text s $$ y = text s <> (empty $$ nest (-length s) y)
                    = text s <> nest (-length s) y
-}

-- ---------------------------------------------------------------------------
-- Operator fixity

infixl 6 <>
infixl 6 <+>
infixl 5 $$, $+$

-- ---------------------------------------------------------------------------
-- The MDoc data type

-- An Doc is a Marked Doc (MDoc) with no interesting markings.
type Doc = MDoc ()

-- | The abstract type of documents.
-- An MDoc represents a *set* of layouts.  A MDoc with
-- no occurrences of Union or NoDoc represents just one layout.
data MDoc a
  = Empty                                                     -- empty
  | NilAbove (MDoc a)                                         -- text "" $$ x
  | TextBeside !(TextDetails a) {-# UNPACK #-} !Int (MDoc a)  -- text s <> x
  | Nest {-# UNPACK #-} !Int (MDoc a)                         -- nest k x
  | Union (MDoc a) (MDoc a)                                   -- ul `union` ur
  | NoDoc                                                     -- The empty set of documents
  | Beside (MDoc a) Bool (MDoc a)                             -- True <=> space between
  | Above (MDoc a) Bool (MDoc a)                              -- True <=> never overlap
#if __GLASGOW_HASKELL__ >= 701
  deriving (Generic)
#endif

{-
Here are the invariants:

1) The argument of NilAbove is never Empty. Therefore
   a NilAbove occupies at least two lines.

2) The argument of @TextBeside@ is never @Nest@.

3) The layouts of the two arguments of @Union@ both flatten to the same
   string.

4) The arguments of @Union@ are either @TextBeside@, or @NilAbove@.

5) A @NoDoc@ may only appear on the first line of the left argument of an
   union. Therefore, the right argument of an union can never be equivalent
   to the empty set (@NoDoc@).

6) An empty document is always represented by @Empty@.  It can't be
   hidden inside a @Nest@, or a @Union@ of two @Empty@s.

7) The first line of every layout in the left argument of @Union@ is
   longer than the first line of any layout in the right argument.
   (1) ensures that the left argument has a first line.  In view of
   (3), this invariant means that the right argument must have at
   least two lines.

Notice the difference between
   * NoDoc (no documents)
   * Empty (one empty document; no height and no width)
   * text "" (a document containing the empty string;
              one line high, but has no width)
-}


-- | RDoc is a "reduced GDoc", guaranteed not to have a top-level Above or Beside.
type RDoc a = MDoc a

-- | The TextDetails data type
--
-- A TextDetails represents a fragment of text that will be
-- output at some point.
data TextDetails a = Chr  {-# UNPACK #-} !Char -- ^ A single Char fragment
                   | Str  String -- ^ A whole String fragment
                   | PStr String -- ^ Used to represent a Fast String fragment
                                 --   but now deprecated and identical to the
                                 --   Str constructor.
                   | Mark a
#if __GLASGOW_HASKELL__ >= 701
                 deriving (Show, Eq, Generic)
#endif

-- Combining @MDoc@ values
instance Monoid (MDoc a) where
    mempty  = empty
    mappend = (<>)

instance IsString (MDoc a) where
    fromString = text

instance Show (MDoc a) where
  showsPrec _ doc cont = fullRender (mode style) (lineLength style)
                                    (ribbonsPerLine style)
                                    txtPrinter cont doc

instance Eq (MDoc a) where
  (==) = (==) `on` render

instance NFData a => NFData (MDoc a) where
  rnf Empty               = ()
  rnf (NilAbove d)        = rnf d
  rnf (TextBeside td i d) = rnf td `seq` rnf i `seq` rnf d
  rnf (Nest k d)          = rnf k  `seq` rnf d
  rnf (Union ur ul)       = rnf ur `seq` rnf ul
  rnf NoDoc               = ()
  rnf (Beside ld s rd)    = rnf ld `seq` rnf s `seq` rnf rd
  rnf (Above ud s ld)     = rnf ud `seq` rnf s `seq` rnf ld

instance NFData a => NFData (TextDetails a) where
  rnf (Chr c)    = rnf c
  rnf (Str str)  = rnf str
  rnf (PStr str) = rnf str
  rnf (Mark a)   = rnf a

-- ---------------------------------------------------------------------------
-- Values and Predicates on GDocs and TextDetails

-- | A document of height and width 1, containing a literal character.
char :: Char -> MDoc a
char c = textBeside_ (Chr c) 1 Empty

-- | A document of height 1 containing a literal string.
-- 'text' satisfies the following laws:
--
-- * @'text' s '<>' 'text' t = 'text' (s'++'t)@
--
-- * @'text' \"\" '<>' x = x@, if @x@ non-empty
--
-- The side condition on the last law is necessary because @'text' \"\"@
-- has height 1, while 'empty' has no height.
text :: String -> MDoc a
text s = case length s of {sl -> textBeside_ (Str s)  sl Empty}

-- | Same as @text@. Used to be used for Bytestrings.
ptext :: String -> MDoc a
ptext s = case length s of {sl -> textBeside_ (PStr s) sl Empty}

-- | Some text with any width. (@text s = sizedText (length s) s@)
sizedText :: Int -> String -> MDoc a
sizedText l s = textBeside_ (Str s) l Empty

-- | Some text, but without any width. Use for non-printing text
-- such as a HTML or Latex tags
zeroWidthText :: String -> MDoc a
zeroWidthText = sizedText 0

-- | The empty document, with no height and no width.
-- 'empty' is the identity for '<>', '<+>', '$$' and '$+$', and anywhere
-- in the argument list for 'sep', 'hcat', 'hsep', 'vcat', 'fcat' etc.
empty :: MDoc a
empty = Empty

-- | Returns 'True' if the document is empty
isEmpty :: MDoc a -> Bool
isEmpty Empty = True
isEmpty _     = False

-- | Produce spacing for indenting the amount specified.
--
-- an old version inserted tabs being 8 columns apart in the output.
indent :: Int -> String
indent !n = replicate n ' '

{-
Q: What is the reason for negative indentation (i.e. argument to indent
   is < 0) ?

A:
This indicates an error in the library client's code.
If we compose a <> b, and the first line of b is more indented than some
other lines of b, the law <n6> (<> eats nests) may cause the pretty
printer to produce an invalid layout:

doc       |0123345
------------------
d1        |a...|
d2        |...b|
          |c...|

d1<>d2    |ab..|
         c|....|

Consider a <> b, let `s' be the length of the last line of `a', `k' the
indentation of the first line of b, and `k0' the indentation of the
left-most line b_i of b.

The produced layout will have negative indentation if `k - k0 > s', as
the first line of b will be put on the (s+1)th column, effectively
translating b horizontally by (k-s). Now if the i^th line of b has an
indentation k0 < (k-s), it is translated out-of-page, causing
`negative indentation'.
-}


semi   :: MDoc a -- ^ A ';' character
comma  :: MDoc a -- ^ A ',' character
colon  :: MDoc a -- ^ A ':' character
space  :: MDoc a -- ^ A space character
equals :: MDoc a -- ^ A '=' character
lparen :: MDoc a -- ^ A '(' character
rparen :: MDoc a -- ^ A ')' character
lbrack :: MDoc a -- ^ A '[' character
rbrack :: MDoc a -- ^ A ']' character
lbrace :: MDoc a -- ^ A '{' character
rbrace :: MDoc a -- ^ A '}' character
semi   = char ';'
comma  = char ','
colon  = char ':'
space  = char ' '
equals = char '='
lparen = char '('
rparen = char ')'
lbrack = char '['
rbrack = char ']'
lbrace = char '{'
rbrace = char '}'

spaceText, nlText :: TextDetails a
spaceText = Chr ' '
nlText    = Chr '\n'

int      :: Int      -> MDoc a -- ^ @int n = text (show n)@
integer  :: Integer  -> MDoc a -- ^ @integer n = text (show n)@
float    :: Float    -> MDoc a -- ^ @float n = text (show n)@
double   :: Double   -> MDoc a -- ^ @double n = text (show n)@
rational :: Rational -> MDoc a -- ^ @rational n = text (show n)@
int      n = text (show n)
integer  n = text (show n)
float    n = text (show n)
double   n = text (show n)
rational n = text (show n)

parens       :: MDoc a -> MDoc a -- ^ Wrap document in @(...)@
brackets     :: MDoc a -> MDoc a -- ^ Wrap document in @[...]@
braces       :: MDoc a -> MDoc a -- ^ Wrap document in @{...}@
quotes       :: MDoc a -> MDoc a -- ^ Wrap document in @\'...\'@
doubleQuotes :: MDoc a -> MDoc a -- ^ Wrap document in @\"...\"@
quotes p       = char '\'' <> p <> char '\''
doubleQuotes p = char '"' <> p <> char '"'
parens p       = char '(' <> p <> char ')'
brackets p     = char '[' <> p <> char ']'
braces p       = char '{' <> p <> char '}'

-- | Apply 'parens' to 'MDoc' if boolean is true.
maybeParens :: Bool -> MDoc a -> MDoc a
maybeParens False = id
maybeParens True = parens

-- | Apply 'brackets' to 'MDoc' if boolean is true.
maybeBrackets :: Bool -> MDoc a -> MDoc a
maybeBrackets False = id
maybeBrackets True = brackets

-- | Apply 'braces' to 'MDoc' if boolean is true.
maybeBraces :: Bool -> MDoc a -> MDoc a
maybeBraces False = id
maybeBraces True = braces

-- | Apply 'quotes' to 'MDoc' if boolean is true.
maybeQuotes :: Bool -> MDoc a -> MDoc a
maybeQuotes False = id
maybeQuotes True = quotes

-- | Apply 'doubleQuotes' to 'MDoc' if boolean is true.
maybeDoubleQuotes :: Bool -> MDoc a -> MDoc a
maybeDoubleQuotes False = id
maybeDoubleQuotes True = doubleQuotes

-- ---------------------------------------------------------------------------
-- Structural operations on GDocs

-- | Perform some simplification of a built up @GDoc@.
reduceDoc :: MDoc a -> RDoc a
reduceDoc (Beside p g q) = beside p g (reduceDoc q)
reduceDoc (Above  p g q) = above  p g (reduceDoc q)
reduceDoc p              = p

-- | List version of '<>'.
hcat :: [MDoc a] -> MDoc a
hcat = snd . reduceHoriz . foldr (\p q -> Beside p False q) empty

-- | List version of '<+>'.
hsep :: [MDoc a] -> MDoc a
hsep = snd . reduceHoriz . foldr (\p q -> Beside p True q)  empty

-- | List version of '$$'.
vcat :: [MDoc a] -> MDoc a
vcat = snd . reduceVert . foldr (\p q -> Above p False q) empty

-- | Nest (or indent) a document by a given number of positions
-- (which may also be negative).  'nest' satisfies the laws:
--
-- * @'nest' 0 x = x@
--
-- * @'nest' k ('nest' k' x) = 'nest' (k+k') x@
--
-- * @'nest' k (x '<>' y) = 'nest' k z '<>' 'nest' k y@
--
-- * @'nest' k (x '$$' y) = 'nest' k x '$$' 'nest' k y@
--
-- * @'nest' k 'empty' = 'empty'@
--
-- * @x '<>' 'nest' k y = x '<>' y@, if @x@ non-empty
--
-- The side condition on the last law is needed because
-- 'empty' is a left identity for '<>'.
nest :: Int -> MDoc a -> MDoc a
nest k p = mkNest k (reduceDoc p)

-- | @hang d1 n d2 = sep [d1, nest n d2]@
hang :: MDoc a -> Int -> MDoc a -> MDoc a
hang d1 n d2 = sep [d1, nest n d2]

-- | @punctuate p [d1, ... dn] = [d1 \<> p, d2 \<> p, ... dn-1 \<> p, dn]@
punctuate :: MDoc a -> [MDoc a] -> [MDoc a]
punctuate _ []     = []
punctuate p (x:xs) = go x xs
                   where go y []     = [y]
                         go y (z:zs) = (y <> p) : go z zs

-- mkNest checks for Nest's invariant that it doesn't have an Empty inside it
mkNest :: Int -> MDoc a -> MDoc a
mkNest k _ | k `seq` False = undefined
mkNest k (Nest k1 p)       = mkNest (k + k1) p
mkNest _ NoDoc             = NoDoc
mkNest _ Empty             = Empty
mkNest 0 p                 = p
mkNest k p                 = nest_ k p

-- mkUnion checks for an empty document
mkUnion :: MDoc a -> MDoc a -> MDoc a
mkUnion Empty _ = Empty
mkUnion p q     = p `union_` q

data IsEmpty = IsEmpty | NotEmpty

reduceHoriz :: MDoc a -> (IsEmpty, MDoc a)
reduceHoriz (Beside p g q) = eliminateEmpty Beside (snd (reduceHoriz p)) g (reduceHoriz q)
reduceHoriz doc            = (NotEmpty, doc)

reduceVert :: MDoc a -> (IsEmpty, MDoc a)
reduceVert (Above  p g q) = eliminateEmpty Above  (snd (reduceVert p)) g (reduceVert q)
reduceVert doc            = (NotEmpty, doc)

{-# INLINE eliminateEmpty #-}
eliminateEmpty ::
  (MDoc a -> Bool -> MDoc a -> MDoc a) ->
  MDoc a -> Bool -> (IsEmpty, MDoc a) -> (IsEmpty, MDoc a)
eliminateEmpty _    Empty _ q          = q
eliminateEmpty cons p     g q          =
  (NotEmpty,
   -- We're not empty whether or not q is empty, so for laziness-sake,
   -- after checking that p isn't empty, we put the NotEmpty result
   -- outside independent of q. This allows reduceAB to immediately
   -- return the appropriate constructor (Above or Beside) without
   -- forcing the entire nested MDoc. This allows the foldr in vcat,
   -- hsep, and hcat to be lazy on its second argument, avoiding a
   -- stack overflow.
   case q of
     (NotEmpty, q') -> cons p g q'
     (IsEmpty, _) -> p)

nilAbove_ :: RDoc a -> RDoc a
nilAbove_ = NilAbove

-- Arg of a TextBeside is always an RDoc
textBeside_ :: TextDetails a -> Int -> RDoc a -> RDoc a
textBeside_ = TextBeside

nest_ :: Int -> RDoc a -> RDoc a
nest_ = Nest

union_ :: RDoc a -> RDoc a -> RDoc a
union_ = Union


-- ---------------------------------------------------------------------------
-- Vertical composition @$$@

-- | Above, except that if the last line of the first argument stops
-- at least one position before the first line of the second begins,
-- these two lines are overlapped.  For example:
--
-- >    text "hi" $$ nest 5 (text "there")
--
-- lays out as
--
-- >    hi   there
--
-- rather than
--
-- >    hi
-- >         there
--
-- '$$' is associative, with identity 'empty', and also satisfies
--
-- * @(x '$$' y) '<>' z = x '$$' (y '<>' z)@, if @y@ non-empty.
--
($$) :: MDoc a -> MDoc a -> MDoc a
p $$  q = above_ p False q

-- | Above, with no overlapping.
-- '$+$' is associative, with identity 'empty'.
($+$) :: MDoc a -> MDoc a -> MDoc a
p $+$ q = above_ p True q

above_ :: MDoc a -> Bool -> MDoc a -> MDoc a
above_ p _ Empty = p
above_ Empty _ q = q
above_ p g q     = Above p g q

above :: MDoc a -> Bool -> RDoc a -> RDoc a
above (Above p g1 q1)  g2 q2 = above p g1 (above q1 g2 q2)
above p@(Beside{})     g  q  = aboveNest (reduceDoc p) g 0 (reduceDoc q)
above p g q                  = aboveNest p             g 0 (reduceDoc q)

-- Specfication: aboveNest p g k q = p $g$ (nest k q)
aboveNest :: RDoc a -> Bool -> Int -> RDoc a -> RDoc a
aboveNest _                   _ k _ | k `seq` False = undefined
aboveNest NoDoc               _ _ _ = NoDoc
aboveNest (p1 `Union` p2)     g k q = aboveNest p1 g k q `union_`
                                      aboveNest p2 g k q

aboveNest Empty               _ k q = mkNest k q
aboveNest (Nest k1 p)         g k q = nest_ k1 (aboveNest p g (k - k1) q)
                                  -- p can't be Empty, so no need for mkNest

aboveNest (NilAbove p)        g k q = nilAbove_ (aboveNest p g k q)
aboveNest (TextBeside s sl p) g k q = textBeside_ s sl rest
                                    where
                                      !k1  = k - sl
                                      rest = case p of
                                                Empty -> nilAboveNest g k1 q
                                                _     -> aboveNest  p g k1 q
aboveNest (Above {})          _ _ _ = error "aboveNest Above"
aboveNest (Beside {})         _ _ _ = error "aboveNest Beside"

-- Specification: text s <> nilaboveNest g k q
--              = text s <> (text "" $g$ nest k q)
nilAboveNest :: Bool -> Int -> RDoc a -> RDoc a
nilAboveNest _ k _           | k `seq` False = undefined
nilAboveNest _ _ Empty       = Empty
                               -- Here's why the "text s <>" is in the spec!
nilAboveNest g k (Nest k1 q) = nilAboveNest g (k + k1) q
nilAboveNest g k q           | not g && k > 0      -- No newline if no overlap
                             = textBeside_ (Str (indent k)) k q
                             | otherwise           -- Put them really above
                             = nilAbove_ (mkNest k q)


-- ---------------------------------------------------------------------------
-- Horizontal composition @<>@

-- We intentionally avoid Data.Monoid.(<>) here due to interactions of
-- Data.Monoid.(<>) and (<+>).  See
-- http://www.haskell.org/pipermail/libraries/2011-November/017066.html

-- | Beside.
-- '<>' is associative, with identity 'empty'.
(<>) :: MDoc a -> MDoc a -> MDoc a
p <>  q = beside_ p False q

-- | Beside, separated by space, unless one of the arguments is 'empty'.
-- '<+>' is associative, with identity 'empty'.
(<+>) :: MDoc a -> MDoc a -> MDoc a
p <+> q = beside_ p True  q

beside_ :: MDoc a -> Bool -> MDoc a -> MDoc a
beside_ p _ Empty = p
beside_ Empty _ q = q
beside_ p g q     = Beside p g q

-- Specification: beside g p q = p <g> q
beside :: MDoc a -> Bool -> RDoc a -> RDoc a
beside NoDoc               _ _   = NoDoc
beside (p1 `Union` p2)     g q   = beside p1 g q `union_` beside p2 g q
beside Empty               _ q   = q
beside (Nest k p)          g q   = nest_ k $! beside p g q
beside p@(Beside p1 g1 q1) g2 q2
         | g1 == g2              = beside p1 g1 $! beside q1 g2 q2
         | otherwise             = beside (reduceDoc p) g2 q2
beside p@(Above{})         g q   = let !d = reduceDoc p in beside d g q
beside (NilAbove p)        g q   = nilAbove_ $! beside p g q
beside (TextBeside s sl p) g q   = textBeside_ s sl $! rest
                               where
                                  rest = case p of
                                           Empty -> nilBeside g q
                                           _     -> beside p g q

-- Specification: text "" <> nilBeside g p
--              = text "" <g> p
nilBeside :: Bool -> RDoc a -> RDoc a
nilBeside _ Empty         = Empty -- Hence the text "" in the spec
nilBeside g (Nest _ p)    = nilBeside g p
nilBeside g p | g         = textBeside_ spaceText 1 p
              | otherwise = p


-- ---------------------------------------------------------------------------
-- Separate, @sep@

-- Specification: sep ps  = oneLiner (hsep ps)
--                         `union`
--                          vcat ps

-- | Either 'hsep' or 'vcat'.
sep  :: [MDoc a] -> MDoc a
sep = sepX True   -- Separate with spaces

-- | Either 'hcat' or 'vcat'.
cat :: [MDoc a] -> MDoc a
cat = sepX False  -- Don't

sepX :: Bool -> [MDoc a] -> MDoc a
sepX _ []     = empty
sepX x (p:ps) = sep1 x (reduceDoc p) 0 ps


-- Specification: sep1 g k ys = sep (x : map (nest k) ys)
--                            = oneLiner (x <g> nest k (hsep ys))
--                              `union` x $$ nest k (vcat ys)
sep1 :: Bool -> RDoc a -> Int -> [MDoc a] -> RDoc a
sep1 _ _                   k _  | k `seq` False = undefined
sep1 _ NoDoc               _ _  = NoDoc
sep1 g (p `Union` q)       k ys = sep1 g p k ys `union_`
                                  aboveNest q False k (reduceDoc (vcat ys))

sep1 g Empty               k ys = mkNest k (sepX g ys)
sep1 g (Nest n p)          k ys = nest_ n (sep1 g p (k - n) ys)

sep1 _ (NilAbove p)        k ys = nilAbove_
                                  (aboveNest p False k (reduceDoc (vcat ys)))
sep1 g (TextBeside s sl p) k ys = textBeside_ s sl (sepNB g p (k - sl) ys)
sep1 _ (Above {})          _ _  = error "sep1 Above"
sep1 _ (Beside {})         _ _  = error "sep1 Beside"

-- Specification: sepNB p k ys = sep1 (text "" <> p) k ys
-- Called when we have already found some text in the first item
-- We have to eat up nests
sepNB :: Bool -> MDoc a -> Int -> [MDoc a] -> MDoc a
sepNB g (Nest _ p) k ys
  = sepNB g p k ys -- Never triggered, because of invariant (2)
sepNB g Empty k ys
  = oneLiner (nilBeside g (reduceDoc rest)) `mkUnion`
    -- XXX: TODO: PRETTY: Used to use True here (but GHC used False...)
    nilAboveNest False k (reduceDoc (vcat ys))
  where
    rest | g         = hsep ys
         | otherwise = hcat ys
sepNB g p k ys
  = sep1 g p k ys


-- ---------------------------------------------------------------------------
-- @fill@

-- | \"Paragraph fill\" version of 'cat'.
fcat :: [MDoc a] -> MDoc a
fcat = fill False

-- | \"Paragraph fill\" version of 'sep'.
fsep :: [MDoc a] -> MDoc a
fsep = fill True

-- Specification:
--
-- fill g docs = fillIndent 0 docs
--
-- fillIndent k [] = []
-- fillIndent k [p] = p
-- fillIndent k (p1:p2:ps) =
--    oneLiner p1 <g> fillIndent (k + length p1 + g ? 1 : 0)
--                               (remove_nests (oneLiner p2) : ps)
--     `Union`
--    (p1 $*$ nest (-k) (fillIndent 0 ps))
--
-- $*$ is defined for layouts (not Docs) as
-- layout1 $*$ layout2 | hasMoreThanOneLine layout1 = layout1 $$ layout2
--                     | otherwise                  = layout1 $+$ layout2

fill :: Bool -> [MDoc a] -> RDoc a
fill _ []     = empty
fill g (p:ps) = fill1 g (reduceDoc p) 0 ps

fill1 :: Bool -> RDoc a -> Int -> [MDoc a] -> MDoc a
fill1 _ _                   k _  | k `seq` False = undefined
fill1 _ NoDoc               _ _  = NoDoc
fill1 g (p `Union` q)       k ys = fill1 g p k ys `union_`
                                   aboveNest q False k (fill g ys)
fill1 g Empty               k ys = mkNest k (fill g ys)
fill1 g (Nest n p)          k ys = nest_ n (fill1 g p (k - n) ys)
fill1 g (NilAbove p)        k ys = nilAbove_ (aboveNest p False k (fill g ys))
fill1 g (TextBeside s sl p) k ys = textBeside_ s sl (fillNB g p (k - sl) ys)
fill1 _ (Above {})          _ _  = error "fill1 Above"
fill1 _ (Beside {})         _ _  = error "fill1 Beside"

fillNB :: Bool -> MDoc a -> Int -> [MDoc a] -> MDoc a
fillNB _ _           k _  | k `seq` False = undefined
fillNB g (Nest _ p)  k ys   = fillNB g p k ys
                              -- Never triggered, because of invariant (2)
fillNB _ Empty _ []         = Empty
fillNB g Empty k (Empty:ys) = fillNB g Empty k ys
fillNB g Empty k (y:ys)     = fillNBE g k y ys
fillNB g p k ys             = fill1 g p k ys


fillNBE :: Bool -> Int -> MDoc a -> [MDoc a] -> MDoc a
fillNBE g k y ys
  = nilBeside g (fill1 g ((elideNest . oneLiner . reduceDoc) y) k' ys)
    -- XXX: TODO: PRETTY: Used to use True here (but GHC used False...)
    `mkUnion` nilAboveNest False k (fill g (y:ys))
  where k' = if g then k - 1 else k

elideNest :: MDoc a -> MDoc a
elideNest (Nest _ d) = d
elideNest d          = d


-- ---------------------------------------------------------------------------
-- Selecting the best layout

best :: Int     -- Line length
     -> Int     -- Ribbon length
     -> RDoc a
     -> RDoc a  -- No unions in here!
best w0 r = get w0
  where
    get w _ | w == 0 && False = undefined
    get _ Empty               = Empty
    get _ NoDoc               = NoDoc
    get w (NilAbove p)        = nilAbove_ (get w p)
    get w (TextBeside s sl p) = textBeside_ s sl (get1 w sl p)
    get w (Nest k p)          = nest_ k (get (w - k) p)
    get w (p `Union` q)       = nicest w r (get w p) (get w q)
    get _ (Above {})          = error "best get Above"
    get _ (Beside {})         = error "best get Beside"

    get1 w _ _ | w == 0 && False  = undefined
    get1 _ _  Empty               = Empty
    get1 _ _  NoDoc               = NoDoc
    get1 w sl (NilAbove p)        = nilAbove_ (get (w - sl) p)
    get1 w sl (TextBeside t tl p) = textBeside_ t tl (get1 w (sl + tl) p)
    get1 w sl (Nest _ p)          = get1 w sl p
    get1 w sl (p `Union` q)       = nicest1 w r sl (get1 w sl p)
                                                   (get1 w sl q)
    get1 _ _  (Above {})          = error "best get1 Above"
    get1 _ _  (Beside {})         = error "best get1 Beside"

nicest :: Int -> Int -> MDoc a -> MDoc a -> MDoc a
nicest !w !r = nicest1 w r 0

nicest1 :: Int -> Int -> Int -> MDoc a -> MDoc a -> MDoc a
nicest1 !w !r !sl p q | fits ((w `min` r) - sl) p = p
                      | otherwise                 = q

fits :: Int  -- Space available
     -> MDoc a
     -> Bool -- True if *first line* of MDoc fits in space available
fits n _ | n < 0           = False
fits _ NoDoc               = False
fits _ Empty               = True
fits _ (NilAbove _)        = True
fits n (TextBeside _ sl p) = fits (n - sl) p
fits _ (Above {})          = error "fits Above"
fits _ (Beside {})         = error "fits Beside"
fits _ (Union {})          = error "fits Union"
fits _ (Nest {})           = error "fits Nest"

-- | @first@ returns its first argument if it is non-empty, otherwise its second.
first :: MDoc a -> MDoc a -> MDoc a
first p q | nonEmptySet p = p -- unused, because (get OneLineMode) is unused
          | otherwise     = q

nonEmptySet :: MDoc a -> Bool
nonEmptySet NoDoc              = False
nonEmptySet (_ `Union` _)      = True
nonEmptySet Empty              = True
nonEmptySet (NilAbove _)       = True
nonEmptySet (TextBeside _ _ p) = nonEmptySet p
nonEmptySet (Nest _ p)         = nonEmptySet p
nonEmptySet (Above {})         = error "nonEmptySet Above"
nonEmptySet (Beside {})        = error "nonEmptySet Beside"

-- @oneLiner@ returns the one-line members of the given set of @GDoc@s.
oneLiner :: MDoc a -> MDoc a
oneLiner NoDoc               = NoDoc
oneLiner Empty               = Empty
oneLiner (NilAbove _)        = NoDoc
oneLiner (TextBeside s sl p) = textBeside_ s sl (oneLiner p)
oneLiner (Nest k p)          = nest_ k (oneLiner p)
oneLiner (p `Union` _)       = oneLiner p
oneLiner (Above {})          = error "oneLiner Above"
oneLiner (Beside {})         = error "oneLiner Beside"


-- ---------------------------------------------------------------------------
-- Rendering

-- | A rendering style.
data Style
  = Style { mode           :: Mode  -- ^ The rendering mode
          , lineLength     :: Int   -- ^ Length of line, in chars
          , ribbonsPerLine :: Float -- ^ Ratio of line length to ribbon length
          }
#if __GLASGOW_HASKELL__ >= 701
  deriving (Show, Eq, Generic)
#endif

-- | The default style (@mode=PageMode, lineLength=100, ribbonsPerLine=1.5@).
style :: Style
style = Style { lineLength = 100, ribbonsPerLine = 1.5, mode = PageMode }

-- | Rendering mode.
data Mode = PageMode     -- ^ Normal
          | ZigZagMode   -- ^ With zig-zag cuts
          | LeftMode     -- ^ No indentation, infinitely long lines
          | OneLineMode  -- ^ All on one line
#if __GLASGOW_HASKELL__ >= 701
          deriving (Show, Eq, Generic)
#endif

-- | Render the @MDoc@ to a String using the default @Style@.
render :: MDoc a -> String
render = fullRender (mode style) (lineLength style) (ribbonsPerLine style)
                    txtPrinter ""

-- | Render the @MDoc@ to a String using the given @Style@.
renderStyle :: Style -> MDoc a -> String
renderStyle s = fullRender (mode s) (lineLength s) (ribbonsPerLine s)
                txtPrinter ""

-- | Default TextDetails printer
txtPrinter :: TextDetails a -> String -> String
txtPrinter (Chr c)   s  = c:s
txtPrinter (Str s1)  s2 = s1 ++ s2
txtPrinter (PStr s1) s2 = s1 ++ s2
txtPrinter (Mark _)  s2 = s2

-- | The general rendering interface.
fullRender :: Mode                       -- ^ Rendering mode
           -> Int                        -- ^ Line length
           -> Float                      -- ^ Ribbons per line
           -> (TextDetails b -> a -> a)  -- ^ What to do with text
           -> a                          -- ^ What to do at the end
           -> MDoc b                     -- ^ The document
           -> a                          -- ^ Result
fullRender OneLineMode _ _ txt end doc
  = easyDisplay spaceText (\_ y -> y) txt end (reduceDoc doc)
fullRender LeftMode    _ _ txt end doc
  = easyDisplay nlText first txt end (reduceDoc doc)

fullRender m lineLen ribbons txt rest doc
  = display m lineLen ribbonLen txt rest doc'
  where
    doc' = best bestLineLen ribbonLen (reduceDoc doc)

    bestLineLen, ribbonLen :: Int
    ribbonLen   = round (fromIntegral lineLen / ribbons)
    bestLineLen = case m of
                      ZigZagMode -> maxBound
                      _          -> lineLen

easyDisplay :: TextDetails b
             -> (MDoc b -> MDoc b -> MDoc b)
             -> (TextDetails b -> a -> a)
             -> a
             -> MDoc b
             -> a
easyDisplay nlSpaceText choose txt end
  = lay
  where
    lay NoDoc              = error "easyDisplay: NoDoc"
    lay (Union p q)        = lay (choose p q)
    lay (Nest _ p)         = lay p
    lay Empty              = end
    lay (NilAbove p)       = nlSpaceText `txt` lay p
    lay (TextBeside s _ p) = s `txt` lay p
    lay (Above {})         = error "easyDisplay Above"
    lay (Beside {})        = error "easyDisplay Beside"

display :: Mode -> Int -> Int -> (TextDetails b -> a -> a) -> a -> MDoc b -> a
display m !page_width !ribbon_width txt end doc
  = case page_width - ribbon_width of { gap_width ->
    case gap_width `quot` 2 of { shift ->
    let
        lay k _            | k `seq` False = undefined
        lay k (Nest k1 p)  = lay (k + k1) p
        lay _ Empty        = end
        lay k (NilAbove p) = nlText `txt` lay k p
        lay k (TextBeside s sl p)
            = case m of
                    ZigZagMode |  k >= gap_width
                               -> nlText `txt` (
                                  Str (replicate shift '/') `txt` (
                                  nlText `txt`
                                  lay1 (k - shift) s sl p ))

                               |  k < 0
                               -> nlText `txt` (
                                  Str (replicate shift '\\') `txt` (
                                  nlText `txt`
                                  lay1 (k + shift) s sl p ))

                    _ -> lay1 k s sl p
        lay _ (Above {})   = error "display lay Above"
        lay _ (Beside {})  = error "display lay Beside"
        lay _ NoDoc        = error "display lay NoDoc"
        lay _ (Union {})   = error "display lay Union"

        lay1 !k s !sl p    = let !r = k + sl
                             in Str (indent k) `txt` (s `txt` lay2 r p)

        lay2 k _ | k `seq` False   = undefined
        lay2 k (NilAbove p)        = nlText `txt` lay k p
        lay2 k (TextBeside s sl p) = s `txt` lay2 (k + sl) p
        lay2 k (Nest _ p)          = lay2 k p
        lay2 _ Empty               = end
        lay2 _ (Above {})          = error "display lay2 Above"
        lay2 _ (Beside {})         = error "display lay2 Beside"
        lay2 _ NoDoc               = error "display lay2 NoDoc"
        lay2 _ (Union {})          = error "display lay2 Union"
    in
    lay 0 doc
    }}

------------------------------------------------------------------------------

-- | mark inserts a zero width mark into the output document

mark :: a -> MDoc a
mark m = textBeside_ (Mark m) 0 Empty