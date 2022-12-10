{-# LANGUAGE GADTs, FlexibleContexts #-}

import Control.Monad

data FINLANG where
    Num :: Int -> FINLANG
    Boolean :: Bool -> FINLANG
    Id :: String -> FINLANG
    Plus :: FINLANG -> FINLANG -> FINLANG
    Minus :: FINLANG -> FINLANG -> FINLANG
    Mult :: FINLANG -> FINLANG -> FINLANG
    Div :: FINLANG -> FINLANG -> FINLANG
    Lambda :: String -> TLANG -> FINLANG -> FINLANG
    App :: FINLANG -> FINLANG -> FINLANG
    Bind :: String -> FINLANG -> FINLANG -> FINLANG
    If :: FINLANG -> FINLANG -> FINLANG -> FINLANG
    And :: FINLANG -> FINLANG -> FINLANG
    Or :: FINLANG -> FINLANG -> FINLANG
    Leq :: FINLANG -> FINLANG -> FINLANG
    IsZero :: FINLANG -> FINLANG
    Fix :: FINLANG -> FINLANG
    List :: FINLANG -> FINLANG -> FINLANG
    Head :: FINLANG -> FINLANG
    Tail :: FINLANG -> FINLANG
    Prepend :: FINLANG -> FINLANG -> FINLANG
    deriving (Show,Eq)

data TLANG where
    TNum :: TLANG
    TBool :: TLANG
    (:->:) :: TLANG -> TLANG -> TLANG
    TList :: Int -> TLANG -> TLANG
    deriving (Show,Eq)

data VLANG where
    NumV :: Int -> VLANG
    BooleanV :: Bool -> VLANG
    ClosureV :: String -> FINLANG -> Env -> VLANG
    ListV :: Int -> VLANG -> VLANG -> VLANG
    deriving (Show,Eq)

type Ctx = [(String, TLANG)]
type Env = [(String, VLANG)]

subst :: String -> FINLANG -> FINLANG -> FINLANG
subst i v (Num x) = (Num x)
subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))
subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))
subst i v (Mult l r) = (Mult (subst i v l) (subst i v r))
subst i v (Div l r) = (Div (subst i v l) (subst i v r))
subst i v (Bind i' v' b') = 
    if i == i' 
    then (Bind i' (subst i v v') b') 
    else (Bind i' (subst i v v') (subst i v b'))
subst i v (Lambda i' t' b') = (Lambda i' t' (subst i v b'))
subst i v (App f' a') = (App (subst i v f') (subst i v a') )
subst i v (Id i') = 
    if i == i' 
    then v 
    else (Id i')
subst i v (Boolean x) = (Boolean x)
subst i v (And l r) = (And (subst i v l) (subst i v r))
subst i v (Or l r) = (Or (subst i v l) (subst i v r))
subst i v (Leq l r) = (Leq (subst i v l) (subst i v r))
subst i v (IsZero x) = (IsZero (subst i v x))
subst i v (If c t f) = (If (subst i v c) (subst i v t) (subst i v f))
subst i v (Fix f) = Fix (subst i v f)


-- Type inference functions
typeof :: Ctx -> FINLANG -> Maybe TLANG
typeof g (Num x) =
    if x < 0
    then Nothing
    else return TNum
typeof g (Boolean x) = Just TBool
typeof g (Id x) = lookup x g
typeof g (Plus x y) = do {
    TNum <- typeof g x;
    TNum <- typeof g y;
        return TNum
}
typeof g (Minus x y) = do {
    TNum <- typeof g x;
    TNum <- typeof g y;
        return TNum
}
typeof g (Mult x y) = do {
    TNum <- typeof g x;
    TNum <- typeof g y;
        return TNum
}
typeof g (Div x y) = do {
    TNum <- typeof g x;
    TNum <- typeof g y;
        return TNum
}
typeof g (Lambda i d b) = do {
    r <- typeof ((i,d):g) b;
        return (d :->: r)
}
typeof g (App x y) = do {
    y' <- typeof g y;
    d :->: r <- typeof g x;
        if y' == d
        then return r
        else Nothing
}
typeof g (Bind x y z) = do {
    y' <- typeof g y;
        typeof ((x,y'):g) z
}
typeof g (If x y z) = do {
    TBool <- typeof g x;
    y' <- typeof g y;
    z' <- typeof g z;
        if y' == z'
        then return y'
        else Nothing
}
typeof g (And x y) = do {
    TBool <- typeof g x;
    TBool <- typeof g y;
        return TBool
}
typeof g (Or x y) = do {
    TBool <- typeof g x;
    TBool <- typeof g y;
        return TBool
}
typeof g (Leq x y) = do {
    TNum <- typeof g x;
    TNum <- typeof g y;
        return TBool
}
typeof g (IsZero x) = do {
    TNum <- typeof g x;
        return TBool
}
typeof g (Fix x) = do {
    (d :->: r) <- typeof g x;
    return r;
}
typeof g (List x y) = do {
    tx <- typeof g x;
    ty <- typeof g y;
        if tx == ty
        then return (TList 2 tx) 
        else Nothing
}
typeof g (Head x) = do {
    TList s t <- typeof g x;
        return t
}
typeof g (Tail x) = do {
    TList s t <- typeof g x;
        if s == 2
        then return t
        else return (TList (s-1) t)
}
typeof g (Prepend x y) = do { --can only prepend a value of the same type as the list (cannot prepend, for example, (Tlist TNum) to (TList TNum), would only be able to prepend TNum)
    TList s t <- typeof g y;
    tx <- typeof g x;
        if t == tx
        then return (TList (s+1) t)
        else Nothing
}
--typeof _ _ = Nothing

eval :: Env -> FINLANG -> Maybe VLANG
eval e (Num x) =
    if x < 0
    then Nothing
    else Just (NumV x)
eval e (Boolean x) = Just (BooleanV x)
eval e (Id x) = lookup x e
eval e (Plus x y) = do {
    (NumV x') <- eval e x;
    (NumV y') <- eval e y;
        return (NumV (x' + y')) 
}
eval e (Minus x y) = do {
    (NumV x') <- eval e x;
    (NumV y') <- eval e y;
    if (x' - y') < 0
    then Nothing
    else return (NumV (x' - y'))
}
eval e (Mult x y) = do {
    (NumV x') <- eval e x;
    (NumV y') <- eval e y;
        return (NumV (x' * y'))
}
eval e (Div x y) = do {
    (NumV x') <- eval e x;
    (NumV y') <- eval e y;
        if y' == 0
        then Nothing
        else return (NumV (x' `div` y'))
}
eval e (Lambda i d b) = Just (ClosureV i b e)
eval e (App x y) = do {
    (ClosureV i b e') <- eval e x;
    y' <- eval e y;
    eval ((i,y'):e') b
}
eval e (Bind i x b) = do {
    x' <- eval e x;
    eval ((i,x'):e) b
}
eval e (If x y z) = do {
    (BooleanV x') <- eval e x;
    if x' then (eval e y) else (eval e z)
}
eval e (And x y) = do {
    (BooleanV x') <- eval e x;
    (BooleanV y') <- eval e y;
        return (BooleanV (x' && y'))
}
eval e (Or x y) = do {
    (BooleanV x') <- eval e x;
    (BooleanV y') <- eval e y;
        return (BooleanV (x' || y'))
}
eval e (Leq x y) = do {
    (NumV x') <- eval e x;
    (NumV y') <- eval e y;
    return (BooleanV (x' <= y'))
}
eval e (IsZero x) = do {
    (NumV x') <- eval e x;
        return (BooleanV (x' == 0))
}
eval e (Fix x) = do {
    (ClosureV i b e') <- eval e x;
    --typ <- Just TNum;
    eval e' (subst i (Fix (Lambda i TNum b)) b)
}
eval e (List x y) = do {
    x' <- eval e x;
    y' <- eval e y;
        return (ListV 2 x' y')
}
eval e (Head x) = do {
    ListV s a b <- eval e x;
        return a 
}
eval e (Tail x) = do {
    (ListV s a b) <- eval e x;
        return b
}
eval e (Prepend x y) = do {
    (ListV s a b) <- eval e y;
    x' <- eval e x;
        return (ListV (s+1) x' (ListV s a b))
}

-- Interpreter function
interp :: FINLANG -> Maybe VLANG
interp a = 
    let e = [] in
    do {
        typeof e a;
        eval e a; 
    }

-- Test Suite
-- Add tests to the tests variable and run main in GHCI
test1 = Plus (Num 1) (Num 2) --Just NumV 3
test2 = (Bind "f" (Lambda "g" (TNum:->:TNum)
            (Lambda "x" TNum (If (IsZero (Id "x")) (Num 1) (Mult (Id "x") (App (Id "g") (Minus (Id "x") (Num 1)))))))
        (App (Fix (Id "f")) (Num 6))) --6! Just NumV 720

test3 = (If (IsZero (Num 8)) (Plus (Num 1) (Num 2)) (Leq (Num 8) (Num 6))) --Nothing, can't be evaluated since the arguments are different types
test4 = (Leq (Num 8) (Num 6)) --Just BooleanV False
test5 = (List (Num 1) (Num 2)) --Just (ListV 2 (NumV 1) (NumV 2))
test6 = (List (Num 1) (Boolean False)) --Nothing
test7 = (Head (List (Num 1) (Num 2))) --Just NumV 1
test8 = (Prepend (Num 8) (List (Num 3) (Num 2))) --Just (ListV 3 (NumV 8) (ListV (NumV 3) (NumV 2)))
test9 = (Prepend (Boolean False) (List (Num 3) (Num 2))) --Nothing
test10 = (Head (Prepend (Num 8) (List (Num 3) (Num 2)))) --Just NumV 8
test11 = (Tail (List (Num 1) (Num 2))) --Just NumV 2
test12 = (Tail (Prepend (Num 8) (List (Num 1) (Num 2)))) --Just (ListV 2 (NumV 1) (NumV 2))

test = do { 
    print(interp test1);
    print(interp test2);
    print(interp test3);
    print(interp test4);
    print(interp test5);
    print(interp test6);
    print(interp test7);
    print(interp test8);
    print(interp test9);
    print(interp test10);
    print(interp test11);
    print(interp test12);
 }