{-# OPTIONS_GHC -Wall #-}

x :: Integer
x = 3

f :: Integer -> Integer
f r = r + 10

g :: (Integer -> s) -> s
g q = q 99

bla :: anything -> anything
bla j = j

blah :: a -> b -> a
blah x _ = x

h :: Integer -> Integer -> Integer
h p q = (p + q) * 2

gg :: (Integer -> s) -> s
gg = \q -> q 99

data Boolean = T | F
  deriving (Eq, Show)

-- Algebraic Data Type (ADT)
data Shape = Circle Integer | Integer ::: Integer | Triangle Integer Integer Integer
  deriving Show

perimeter :: Shape -> Integer
perimeter sh =
  case sh of
    Circle r -> r * 3 * 2
    w ::: h -> (w + h) * 2
    Triangle a b c -> a + b + c

perimeter2 :: Shape -> Integer
perimeter2 (Circle r) = r * 3 * 2
perimeter2 (w ::: h) = (w + h) * 2
perimeter2 (Triangle a b c) = a + b + c

data Three a = Threee a a a
  deriving (Eq, Show)

multiply :: Three Integer -> Integer
multiply (Threee x y z) = x * y * z

multiply2 :: Three Integer -> Integer
multiply2 is = case is of Threee x y z -> x * y * z

multiply3 :: Three Integer -> Integer
multiply3 = \is -> case is of Threee x y z -> x * y * z

data NaturalNumber = Zero | Successor NaturalNumber
  deriving (Eq, Show)

three :: NaturalNumber
three = Successor (Successor (Successor Zero))

add :: NaturalNumber -> NaturalNumber -> NaturalNumber
add a b =
  case a of
    Zero -> b
    Successor x -> Successor (add x b) -- add x (Successor b)

infinity :: NaturalNumber
infinity = Successor infinity

mult :: NaturalNumber -> NaturalNumber -> NaturalNumber
mult Zero _ = Zero
mult (Successor x) b = add b (mult x b)

{-
exp :: NaturalNumber -> NaturalNumber -> NaturalNumber
greaterthan :: NaturalNumber -> NaturalNumber -> Bool
-}