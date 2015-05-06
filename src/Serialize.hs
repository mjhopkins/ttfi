-- * Essentially, Haskell98
{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances #-}

-- * Serialization and de-serialization in tagless-final style
-- The deserialization problem is posed in
-- \url{http://userpages.uni-koblenz.de/~laemmel/TheEagle/}

module Serialize where

import Intro2 hiding (main)
import Control.Monad

-- Data type of trees representing our expressions `on the wire'
-- Our wire format is essentially JASON
data Tree = Leaf String			-- atom
	  | Node String [Tree]		-- collection
	    deriving (Eq, Read, Show)

-- Serializer for Exp -- just another interpreter, similar
-- to the ones we have seen

instance ExpSYM Tree where
    lit n = Node "Lit" [Leaf $ show n]
    neg e = Node "Neg" [e]
    add e1 e2 = Node "Add" [e1,e2]

toTree :: Tree -> Tree
toTree = id

-- tf1 = add (lit 8) (neg (add (lit 1) (lit 2)))
-- Our sample term
tf1_tree = toTree tf1

-- Looks like a XML/JSON/S-expression...
-- Node "Add" [Node "Lit" [Leaf "8"],
--    Node "Neg" [Node "Add" [Node "Lit" [Leaf "1"],Node "Lit" [Leaf "2"]]]]

-- * //
-- The problem is to write a 
-- * deserializer: take a tree and produce a term
-- * Challenge: maintain multiple interpretations
-- The result can be interpreted in any existing or future interpreter
-- That is a challenging part.

-- The deserializer is necessarily a partial function: the input
-- may be ill-formed. For example: 
-- * possible bad input: Node "Lit" [Leaf "1", Leaf "2"]
-- A literal expression may have only one argument.
-- We use (Either ErrMsg a), the Error monad, to model partiality,
-- reporting error messages.

type ErrMsg = String

safeRead :: Read a => String -> Either ErrMsg a
safeRead s = case reads s of
	      [(x,"")] -> Right x
	      _        -> Left $ "Read error: " ++ s

-- * //


-- Inferred type:
-- fromTree :: (ExpSYM b) => Tree -> Either ErrMsg b
fromTree (Node "Lit" [Leaf n]) = liftM lit $ safeRead n
fromTree (Node "Neg" [e])      = liftM neg $ fromTree e
fromTree (Node "Add" [e1,e2])  = liftM2 add (fromTree e1) (fromTree e2)
fromTree e = Left $ "Invalid tree: " ++ show e

-- We can deserialize our sample term and evaluate it
-- with any of the existing interpreters
tf1'_eval =
    let tf1' = fromTree tf1_tree
    in case tf1' of
        Left  e -> putStrLn $ "Error: " ++ e
	Right x -> print $ eval x
-- 5

-- But we wish to evaluate the de-serialized term several times,
-- with different interpreters

{-
tf1'_evew' = 
    let tf1' = fromTree tf1_tree
    in case tf1' of
        Left  e -> putStrLn $ "Error: " ++ e
	Right x -> do
		   print $ eval x 
		   print $ view x 

And here we get the type error:
    Couldn't match expected type `String' against inferred type `Int'
    In the first argument of `view', namely `x'
    In the second argument of `($)', namely `view x'
    In the expression: print $ view x

What happened? We lost polymorphism! The result of fromTree is
polymorphic in the interpreter: ExpSYM repr => repr
After the pattern-matching, the variable x is no longer polymorphic.
Haskell does not have unfettered first-class polymorphism, 
for a good reason. Thus after the pattern-match in 'case', we can interpret
the result of deserialization only with one interpreter. We have lost
extensibility!
-}

-- * What are the solutions?
-- One is to re-write fromTree to have this signature
-- * fromTree :: String -> Either ErrMsg Wrapped
-- where 
-- * newtype Wrapped = Wrapped{unWrap :: forall repr. ExpSYM repr => repr}
-- emulating first-class polymorphism. The successful case analysis
-- of the parsing result will give us the value of the type Wrapped,
-- which can be interpreted in many ways, as its type indicates.
-- Alas, we lose extensibility again: we can no longer enrich our
-- language because we fixed the constraint ExpSYM. When we later
-- add the multiplication form, we need to add the MulSYM constraint.
-- Thus we have to re-define Wrapped. We could not use any of the
-- existing fromTree code (in its compiled form).

-- Here we show a different solution
-- We introduce a somewhat puzzling interpreter

instance (ExpSYM repr, ExpSYM repr') => ExpSYM (repr,repr') where
    lit x       = (lit x, lit x)
    neg (e1,e2) = (neg e1, neg e2)
    add (e11,e12) (e21, e22) = (add e11 e21, add e12 e22)

duplicate :: (ExpSYM repr, ExpSYM repr') => (repr,repr') -> (repr,repr')
duplicate = id

-- * We check the result of deserialization once
-- On success, we pass the deserialized term to a consumer f
check_consume :: (t -> IO ()) -> Either String t -> IO ()
check_consume f (Left  e) = putStrLn $ "Error: " ++ e
check_consume f (Right x) = f x

-- * Whenever we use a value, we have to duplicate it first,
-- to leave the other copy for different interpreters

dup_consume :: (ExpSYM b, ExpSYM t, Show a) => (t -> a) -> (t, b) -> IO b
dup_consume ev x = print (ev x1) >> return x2
 where (x1,x2) = duplicate x

-- We consume the deserialized value with three different interpreters
tf1'_int3 :: IO ()
tf1'_int3 = check_consume thrice . fromTree $ tf1_tree

thrice :: (Int, (String, Tree)) -> IO ()
thrice x = dup_consume eval x >>= dup_consume view >>= print . toTree

{-
5
"(8 + (-(1 + 2)))"
Node "Add" ...
-}

-- * //
-- Let us write the deserializer in the style of open recursion
-- we shall see the benefit later

-- The signature could have been inferred
fromTreeExt :: (ExpSYM repr) => 
	       (Tree -> Either ErrMsg repr) -> Tree -> Either ErrMsg repr
fromTreeExt self (Node "Lit" [Leaf n]) = liftM lit $ safeRead n
fromTreeExt self (Node "Neg" [e])      = liftM neg $ self e
fromTreeExt self (Node "Add" [e1,e2])  = liftM2 add (self e1) (self e2)
fromTreeExt self e = Left $ "Invalid tree: " ++ show e

-- we use the fixpoint combinator to tie up the knot

fix :: (a -> a) -> a
fix f = f (fix f)

fromTree' :: ExpSYM repr => Tree -> Either ErrMsg repr
fromTree' = fix fromTreeExt		-- One does use fix in real programs


tf1E_int3 = check_consume thrice . fromTree' $ tf1_tree

{-
5
"(8 + (-(1 + 2)))"
Node "Add" ...
-}

-- * Does each evaluation of tf1' re-parses tf1_tree?
-- We try on a bad input
tfxE_int3 = check_consume thrice . fromTree' $ Node "Lit" [Leaf "1", Leaf "2"]
{-
Error: Invalid tree: Node "Lit" [Leaf "1",Leaf "2"]
-}
-- That is, we get and report the parsing error before we started any 
-- interpretation. This implies that the whole parsing is completed
-- before any interpretation starts.

main = do
       print tf1_tree
       tf1'_eval
       tf1'_int3
       tf1E_int3
       tfxE_int3


--  LocalWords:  deserialization Serializer deserializer
