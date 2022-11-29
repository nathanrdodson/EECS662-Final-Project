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
    Lambda :: String -> TYPELANG -> FINLANG -> FINLANG
    App :: FINLANG -> FINLANG -> FINLANG
    Bind :: String -> FINLANG -> FINLANG -> FINLANG
    If :: FINLANG -> FINLANG -> FINLANG -> FINLANG
    And :: FINLANG -> FINLANG -> FINLANG
    Or :: FINLANG -> FINLANG -> FINLANG
    Leq :: FINLANG -> FINLANG -> FINLANG
    IsZero :: FINLANG -> FINLANG
    deriving (Show,Eq)

data TYPELANG where
    TNum :: TYPELANG
    TBool :: TYPELANG
    (:->:) :: TYPELANG -> TYPELANG -> TYPELANG
    deriving (Show,Eq)

type Ctx = [(String, TYPELANG)]
type Env = [(String, FINLANG)]

typeof :: Ctx -> FINLANG -> (Maybe TYPELANG)
typeof g (Num x) =
    if x < 0
    then Nothing
    else return TNum
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

typeof _ _ = Nothing

eval :: Env -> FINLANG -> (Maybe FINLANG)
eval _ _ = Nothing
