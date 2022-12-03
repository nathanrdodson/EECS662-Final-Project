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
    TList :: TLANG
    deriving (Show,Eq)

data VLANG where
    NumV :: Int -> VLANG
    BooleanV :: Bool -> VLANG
    ClosureV :: String -> FINLANG -> Env -> VLANG
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

typeof :: Ctx -> FINLANG -> (Maybe TLANG)
typeof g (Num x) =
    if x < 0
    then Nothing
    else return TNum
typeof g (Boolean x) = Just (TBool)
typeof g (Id x) = (lookup x g)
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
typeof _ _ = Nothing

eval :: Env -> FINLANG -> (Maybe VLANG)
eval e (Num x) =
    if x < 0
    then Nothing
    else Just (NumV x)
eval e (Boolean x) = Just (BooleanV x)
eval e (Id x) = (lookup x e)
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
eval e (Bind x y z) = (eval e (App (Lambda x (TNum) z) y))
eval e (If x y z) = do {
    (BooleanV x') <- eval e x;
    y' <- eval e y;
    z' <- eval e z;
        if x'
        then Just y'
        else Just z'
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
    (ClosureV i b e) <- eval e x;
    ty <- Just TNum;
    eval e (subst i (Fix (Lambda i ty b)) b)
}
eval e (List x y) = eval e (Lambda "x" (TBool) (If (Id "x") x y))
eval e (Head x) = do {
    (ClosureV s1 (If (Id s2) t1 t2) e') <- eval e x;
        if s1 == s2
        then eval ((s1,(BooleanV True)):e') (If (Id s1) t1 t2)
        else Nothing
}
eval e (Tail x) = do {
    (ClosureV s1 (If (Id s2) t1 t2) e') <- eval e x;
        if s1 == s2
        then eval ((s1,(BooleanV False)):e') (If (Id s1) t1 t2)
        else Nothing
}
eval e (Prepend x y) = do {
    (ClosureV s1 (If (Id s2) t1 t2) e') <- eval e y;
        eval e' (Lambda s1 (TBool) (If (Id s1) x (List t1 t2)))
}