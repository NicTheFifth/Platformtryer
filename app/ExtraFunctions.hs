module ExtraFunctions where

infixr *.

(*.) :: (c -> d) -> (a -> b -> c) -> a -> b-> d
(*.) f g x y = f (g x y)