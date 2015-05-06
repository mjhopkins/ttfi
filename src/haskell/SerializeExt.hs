-- * Essentially, Haskell98
{-# LANGUAGE NoMonomorphismRestriction #-}

-- * Serialization and de-serialization in the tagless-final style
-- * for the extended data type
-- The deserialization problem is posed in
-- \url{http://userpages.uni-koblenz.de/~laemmel/TheEagle/}

-- * Solving the expression problem
-- What is the expression problem (see the slides)
-- We can: 
--   add new operations on the data type: we have just added
--   a serializer
--   Add a new expression form (multiplication)
-- We now see how we can extend the serializer and de-serializer.


module SerializeExt where

-- We really are re-using the existing code (which may already be compiled):
import Intro2 hiding (main)
import ExtF hiding (main)	     -- import the extended `variant': Mul

import Serialize (Tree(..))	     -- import the wire format
				     -- import the original serializer
import qualified Serialize as S	hiding (main)
import Control.Monad

-- * //
-- First we extend the serializer

instance MulSYM Tree where
    mul e1 e2 = Node "Mul" [e1,e2]

-- And the puzzling duplicator
instance (MulSYM repr, MulSYM repr') => MulSYM (repr,repr') where
    mul (e11,e12) (e21, e22) = (mul e11 e21, mul e12 e22)

-- And we serialize the extended terms

tfm1_tree = S.toTree tfm1
--  Node "Add" [Node "Lit" [Leaf "7"],
--    Node "Neg" [Node "Mul" [Node "Lit" [Leaf "1"],Node "Lit" [Leaf "2"]]]]

tfm2_tree = S.toTree tfm2
-- Node "Mul" [Node "Lit" [Leaf "7"],
--   Node "Add" [Node "Lit" [Leaf "8"],
--     Node "Neg" [Node "Add" [Node "Lit" [Leaf "1"],Node "Lit" [Leaf "2"]]]]]

-- * //
-- Let us now extend the de-serializer
-- We merely `add' one clause to the de-serializer of unextended terms.
-- We have not touched the code of the old de-serializer. The file
-- Serialize.hs could have been given to us in the compiled form.
-- We don't need the source code for it since we don't modify it and
-- don't recompile it.

-- The inferred signature is exactly as we wish:
-- fromTreeExt
--   :: (MulSYM repr, ExpSYM repr) =>
--      (Tree -> Either S.ErrMsg repr) -> Tree -> Either S.ErrMsg repr

-- This is a different function, from S.fromTreeExt
-- It relays to the latter for all other nodes
fromTreeExt self (Node "Mul" [e1,e2]) = liftM2 mul (self e1) (self e2)
fromTreeExt self e = S.fromTreeExt self e -- use the old one for the rest

-- * Tie up the knot
fromTree = S.fix fromTreeExt		-- One does use fix in real programs

-- Now we can see the real benefit of using fix in real programs.
-- The fixpoint combinator is NOT a mere curiosity

-- We can de-serialize the unextended terms using the extended
-- de-serializer

tf1'_int3 = S.check_consume S.thrice . fromTree $ S.tf1_tree
{-
5
"(8 + (-(1 + 2)))"
Node "Add" ...
-}

-- We can now de-serialize the extended terms
-- And evaluate them in different interpreters
tfm1'_int3 = S.check_consume S.thrice . fromTree $ tfm1_tree
{-
5
"(7 + (-(1 * 2)))"
Node "Add" [Node "Lit" [Leaf "7"] ...
-}

tfm2'_int3 = S.check_consume S.thrice . fromTree $ tfm2_tree
{-
35
"(7 * (8 + (-(1 + 2))))"
Node "Mul" ...
-}


-- * Extending the deserializer has been an open problem!

-- Of course we had to write the deserializer in the open-recursion form.
-- We had to anticipate the extension.
-- But we had to extend the wire format, which is the 
-- input of the deserializer (rather than the expression, which
-- is the output of the deserializer). 
-- Whether we use the final tagless approach or not,
-- if we want deserializer to be extensible with respect to its
-- input (the wire format), we have to explicitly make it so.


main = do
       print tfm1_tree
       print tfm2_tree
       tf1'_int3
       tfm1'_int3
       tfm2'_int3

--  LocalWords:  serializer deserializer
