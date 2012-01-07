{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Lol.FD (
    -- Types
    FD,           -- Monad for finite domain constraint solver
    FDVar,        -- Finite domain solver variable

    -- Functions
    runFD,        -- Run the monad and return a list of solutions.
    newVar,       -- Create a new FDVar
    newVars,      -- Create multiple FDVars
    hasValue,     -- Constrain a FDVar to a specific value
    hasValues,    -- Constrain a FDVar to a list of values
    lacksValue,   -- Constrain a FDVar to not have a value
    lacksValues,  -- Constrain a FDVar to not have any of a list of values
    satisfies,    -- Constrain a FDVar to satisfy a predicate
    same,         -- Constrain two FDVars to be the same
    different,    -- Constrain two FDVars to be different
    allDifferent, -- Constrain a list of FDVars to be different
    (.<.),        -- Constrain one FDVar to be less than another
    (.<=.),       -- Constrain one FDVar to be less than or equal to another
    orderedEx,    -- Constrain a list of FDVars to be exclusively ordered
    orderedIn,    -- Constrain a list of FDVars to be inclusively ordered
    labelling     -- Backtracking search for all solutions
    ) where

import Prelude hiding (lookup)
import Control.Monad.State.Lazy
import qualified Data.Map as Map
import Data.Map ((!), Map)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)

-- The FD monad
newtype FD s a = FD { unFD :: StateT (FDState s) [] a }
    deriving (Monad, MonadPlus, MonadState (FDState s))

-- FD variables
newtype FDVar s = FDVar { unFDVar :: Int } deriving (Ord, Eq)

type VarSupply s = FDVar s
data VarInfo s = VarInfo
     { delayedConstraints :: FD s (), values :: IntSet }
type VarMap s = Map (FDVar s) (VarInfo s)
data FDState s = FDState
     { varSupply :: VarSupply s, varMap :: VarMap s }

-- | Run the FD monad and produce a lazy list of possible solutions.
runFD :: (forall s . FD s a) -> [a]
runFD fd = evalStateT (unFD fd) initState

initState :: FDState s
initState = FDState { varSupply = FDVar 0, varMap = Map.empty }

-- Get a new FDVar
newVar :: (Enum a) => [a] -> FD s (FDVar s)
newVar domain = do
    v <- nextVar
    v `isOneOf` domain
    return v
    where
        nextVar :: FD s (FDVar s)
        nextVar = do
            s <- get
            let v = varSupply s
            put $ s { varSupply = FDVar (unFDVar v + 1) }
            return v
        isOneOf :: (Enum a) => FDVar s -> [a] -> FD s ()
        x `isOneOf` d =
            modify $ \s ->
                let vm = varMap s
                    vi = VarInfo {
                        delayedConstraints = return (),
                        values = IntSet.fromList $ map fromEnum d}
                in
                s { varMap = Map.insert x vi vm }

newVars :: (Enum a) => Int -> [a] -> FD s [FDVar s]
newVars n domain = replicateM n (newVar domain)

-- Lookup the current domain of a variable.
lookup :: FDVar s -> FD s IntSet
lookup x = do
    s <- get
    return . values $ varMap s ! x

-- Update the domain of a variable and fire all delayed constraints
-- associated with that variable.
update :: FDVar s -> IntSet -> FD s ()
update x i = do
    s <- get
    let vm = varMap s
    let vi = vm ! x
    put $ s { varMap = Map.insert x (vi { values = i}) vm }
    delayedConstraints vi

-- Add a new constraint for a variable to the constraint store.
addConstraint :: FDVar s -> FD s () -> FD s ()
addConstraint x constraint = do
    s <- get
    let vm = varMap s
    let vi = vm ! x
    let cs = delayedConstraints vi
    put $ s { varMap =
        Map.insert x (vi { delayedConstraints = cs >> constraint }) vm }
 
-- Useful helper function for adding binary constraints between FDVars.
type BinaryConstraint s = FDVar s -> FDVar s -> FD s ()
addBinaryConstraint :: BinaryConstraint s -> BinaryConstraint s
addBinaryConstraint f x y = do
    let constraint  = f x y
    constraint
    addConstraint x constraint
    addConstraint y constraint

-- Constrain a variable to a particular value.
hasValue :: (Enum a) => FDVar s -> a -> FD s ()
var `hasValue` val = do
    vals <- lookup var
    guard $ fromEnum val `IntSet.member` vals
    let i = IntSet.singleton $ fromEnum val
    when (i /= vals) $ update var i

-- Constrain a variable to a list of values.
hasValues :: (Enum a) => FDVar s -> [a] -> FD s ()
var `hasValues` val = do
    vals <- lookup var
    guard $ all (\x -> fromEnum x `IntSet.member` vals) val
    let i = IntSet.fromList $ map fromEnum val
    when (i /= vals) $ update var i

-- Constrain a variable to not have a value.
lacksValue :: (Enum a) => FDVar s -> a -> FD s ()
var `lacksValue` val = do
    vals <- lookup var
    let i = IntSet.delete (fromEnum val) vals
    guard $ not $ IntSet.null i
    when (i /= vals) $ update var i

-- Constrain a variable to not have any values in a list.
lacksValues :: (Enum a) => FDVar s -> [a] -> FD s ()
var `lacksValues` vals = mapM_ (lacksValue var) vals

-- Constrain a variable with a filter-style predicate.
satisfies :: (Enum a) => FDVar s -> (a -> Bool) -> FD s ()
var `satisfies` pred = do
    vals <- lookup var
    let i = IntSet.filter (pred . toEnum) vals
    guard $ not $ IntSet.null i
    when (i /= vals) $ update var i

-- Constrain two variables to have the same value.
same :: FDVar s -> FDVar s -> FD s ()
same = addBinaryConstraint $ \x y -> do
    xv <- lookup x
    yv <- lookup y
    let i = IntSet.intersection xv yv
    guard $ not $ IntSet.null i
    when (i /= xv) $ update x i
    when (i /= yv) $ update y i

-- Constrain two variables to have different values.
different :: FDVar s -> FDVar s -> FD s ()
different = addBinaryConstraint $ \x y -> do
    xv <- lookup x
    yv <- lookup y
    guard $ IntSet.size xv > 1 || IntSet.size yv > 1 || xv /= yv
    when (IntSet.size xv == 1 && xv `IntSet.isProperSubsetOf` yv) $
        update y (yv `IntSet.difference` xv)
    when (IntSet.size yv == 1 && yv `IntSet.isProperSubsetOf` xv) $
        update x (xv `IntSet.difference` yv)

-- Constrain a list of variables to all have different values.
allDifferent :: [FDVar s] -> FD s ()
allDifferent (x:xs) = do
    mapM_ (different x) xs
    allDifferent xs
allDifferent _ = return ()

-- Constrain one variable to have a value less than the value of another
-- variable.
(.<.) :: FDVar s -> FDVar s -> FD s ()
(.<.) = addBinaryConstraint $ \x y -> do
    xv <- lookup x
    yv <- lookup y
    let xv' = IntSet.filter (< IntSet.findMax yv) xv
    let yv' = IntSet.filter (> IntSet.findMin xv) yv
    guard $ not $ IntSet.null xv'
    guard $ not $ IntSet.null yv'
    when (xv /= xv') $ update x xv'
    when (yv /= yv') $ update y yv'

-- Same as above, but the values can be equal too.
(.<=.) :: FDVar s -> FDVar s -> FD s ()
(.<=.) = addBinaryConstraint $ \x y -> do
    xv <- lookup x
    yv <- lookup y
    let xv' = IntSet.filter (<= IntSet.findMax yv) xv
    let yv' = IntSet.filter (>= IntSet.findMin xv) yv
    guard $ not $ IntSet.null xv'
    guard $ not $ IntSet.null yv'
    when (xv /= xv') $ update x xv'
    when (yv /= yv') $ update y yv'

-- | Force variables to all be ordered according to order of enumeration,
--   exclusive. This means that [a, b, c..] will satisfy a < b < c ...
orderedEx :: [FDVar s] -> FD s ()
orderedEx xs = zipWithM_ (.<.) xs (tail xs)

-- Same as above, but inclusive.
orderedIn :: [FDVar s] -> FD s ()
orderedIn xs = zipWithM_ (.<=.) xs (tail xs)

-- Label variables using a depth-first left-to-right search.
labelling :: (Enum a) => [FDVar s] -> FD s [a]
labelling = mapM label where
    label var = do
        vals <- lookup var
        val <- FD . lift $ IntSet.toList vals
        var `hasValue` val
        return $ toEnum val
