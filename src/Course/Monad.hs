{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Monad where

import Course.Applicative
import Course.Core
import Course.ExactlyOne
import Course.Functor
import Course.List
import Course.Optional
import qualified Prelude as P((=<<))

-- | All instances of the `Monad` type-class must satisfy one law. This law
-- is not checked by the compiler. This law is given as:
--
-- * The law of associativity
--   `∀f g x. g =<< (f =<< x) ≅ ((g =<<) . f) =<< x`
class Applicative f => Monad f where
  -- Pronounced, bind.
  (=<<) ::
    (a -> f b)
    -> f a
    -> f b

infixr 1 =<<

-- | Binds a function on the ExactlyOne monad.
--
-- >>> (\x -> ExactlyOne(x+1)) =<< ExactlyOne 2
-- ExactlyOne 3
instance Monad ExactlyOne where
  (=<<) ::
    (a -> ExactlyOne b)
    -> ExactlyOne a
    -> ExactlyOne b
  (=<<) =
    error "todo: Course.Monad (=<<)#instance ExactlyOne"

-- | Binds a function on a List.
--
-- >>> (\n -> n :. n :. Nil) =<< (1 :. 2 :. 3 :. Nil)
-- [1,1,2,2,3,3]
instance Monad List where
  (=<<) ::
    (a -> List b) -> List a -> List b
  (=<<) = flatMap

{-
(=<<) _ Nil = Nil
(=<<) f (h :. t) = f h ++ (=<<) f t
-}

{-
(=<<) = foldRight ((++) . f) Nil
-}

-- | Binds a function on an Optional.
--
-- >>> (\n -> Full (n + n)) =<< Full 7
-- Full 14
instance Monad Optional where
  (=<<) ::
    (a -> Optional b) -> Optional a -> Optional b
  (=<<) = bindOptional

-- | Binds a function on the reader ((->) t).
--
-- >>> ((*) =<< (+10)) 7
-- 119
instance Monad ((->) t) where
  (=<<) ::
    (a -> ((->) t b)) -> ((->) t a) -> ((->) t b)
  (=<<) a2t2b t2a t = a2t2b (t2a t) t

{-
a2t2b (t2a t) t is S flipped
-}

{-
this is the reader monad
and t is the configuration object to be passed in dependency injection
-}

-- | Witness that all things with (=<<) and (<$>) also have (<*>).
--
-- >>> ExactlyOne (+10) <**> ExactlyOne 8
-- ExactlyOne 18
--
-- >>> (+1) :. (*2) :. Nil <**> 1 :. 2 :. 3 :. Nil
-- [2,3,4,2,4,6]
--
-- >>> Full (+8) <**> Full 7
-- Full 15
--
-- >>> Empty <**> Full 7
-- Empty
--
-- >>> Full (+8) <**> Empty
-- Empty
--
-- >>> ((+) <**> (+10)) 3
-- 16
--
-- >>> ((+) <**> (+5)) 3
-- 11
--
-- >>> ((+) <**> (+5)) 1
-- 7
--
-- >>> ((*) <**> (+10)) 3
-- 39
--
-- >>> ((*) <**> (+2)) 3
-- 15
(<**>) ::
  Monad f => f (a -> b) -> f a -> f b
(<**>) =
  \f_a2b f_a ->
    f_a2b >>= \a2b ->
    f_a >>= \a ->
    pure (a2b a)

{-
f x -> (x -> f y) -> f y                -- type of bind
f (a -> b) -> ((a -> b) -> f y) -> f y  -- x can be a function
f a -> (a -> f b) -> f b
-}

{-
lift2 id
-}

infixl 4 <**>

-- | Flattens a combined structure to a single structure.
--
-- >>> join ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
-- [1,2,3,1,2]
--
-- >>> join (Full Empty)
-- Empty
--
-- >>> join (Full (Full 7))
-- Full 7
--
-- >>> join (+) 7
-- 14
join ::
  Monad f => f (f a) -> f a
join = (=<<) id

{-
f x -> (x -> f y) -> f y
f (f a) -> (f a -> f a) -> f a

join f_f_a = (>>=) f_f_a id
-}

{-
bynd :: Monad f => (a -> f b) -> f a -> f b
bynd f f_a = join (f <$> f_a)
-}

-- | Implement a flipped version of @(=<<)@, however, use only
-- @join@ and @(<$>)@.
-- Pronounced, bind flipped.
--
-- >>> ((+10) >>= (*)) 7
-- 119
(>>=) ::
  Monad f => f a -> (a -> f b) -> f b
(>>=) = flip (=<<)

infixl 1 >>=

-- | Implement composition within the @Monad@ environment.
-- Pronounced, kleisli composition.
--
-- >>> ((\n -> n :. n :. Nil) <=< (\n -> n+1 :. n+2 :. Nil)) 1
-- [2,2,3,3]
(<=<) ::
  Monad f => (b -> f c) -> (a -> f b) -> a -> f c
(<=<) =
  \b2fc -> \a2fb -> \a -> (>>=) (a2fb a) b2fc

{-
type of bind: f x -> (x -> f y) -> f y
-}

infixr 1 <=<

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Monad IO where
  (=<<) =
    (P.=<<)
