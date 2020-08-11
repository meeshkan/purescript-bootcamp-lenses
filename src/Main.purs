module Main where

import Prelude
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class.Console (log)
import Global (readFloat, readInt)

{-
  ****PSEUDO***
  Use of lenses
  -------------
  Zoom into the value  ~~ { a: 1 } -> 1
  Zoom into the type   ~~  data Foo = Bar | Baz -> Baz
  Zoom into the constructor ~~ data Foo a = Foo a -> a     ~~ comonad :: Newtype unwrap
  Change the microsope ~~ A -> B -> A
  Apply a filter       ~~ Instagram (kinda like convolution)

  Result
  ------
  Fold   ::::: Getter
  Setter

  Use of lenses [Optics]  p a b -> p s t
  -------------
  Lens         ~~~~~~~~~~ Strong -> Strong
  Prism        ~~~~~~~~~~ Choice -> Choice
  Traversal    ~~~~~~~~~~ Wander -> Wander
  Iso          ~~~~~~~~~~ Profunctor -> Profunctor
  Grate        ~~~~~~~~~~ Closed -> Closed

  Result
  ------
  Fold (Getter)   Forget
  Setter   Function
-}
---- profunctor
---- C_op x D -> Set
---- profunctor is generalized i/o
-- map (a -> b) -> f a -> f b
class Profunctor p where
  dimap :: forall a b s t. (s -> a) -> (b -> t) -> p a b -> p s t

class
  Profunctor p <= Strong p where
  first :: forall a b c. p a b -> p (Tuple a c) (Tuple b c)
  second :: forall a b c. p a b -> p (Tuple c a) (Tuple c b)

class
  Profunctor p <= Choice p where
  left :: forall a b c. p a b -> p (Either a c) (Either b c)
  right :: forall a b c. p a b -> p (Either c a) (Either c b)

class
  Profunctor p <= Closed p where
  close :: forall a b x. p a b -> p (x -> a) (x -> b)

-- traverse :: forall a b m. Applicative m => (a -> m b) -> t a -> m (t b)
class
  Profunctor p <= Wander p where
  wander :: forall s t a b. (forall f. Applicative f => (a -> f b) -> s -> f t) -> p a b -> p s t

instance functionProfunctor :: Profunctor Function where
  dimap i o f = o <<< f <<< i

instance functionStrong :: Strong Function where
  first f (Tuple x y) = Tuple (f x) y
  second f (Tuple x y) = Tuple x (f y)

instance functionChoice :: Choice Function where
  left f (Left a) = (Left $ f a)
  left f (Right c) = (Right c)
  right f (Left c) = (Left c)
  right f (Right a) = (Right $ f a)

instance functionWander :: Wander Function where
  wander f ab s = go ((f (\a -> Identity $ ab a)) s)
    where
    go (Identity x) = x

instance functionClosed :: Closed Function where
  close ab = (\xa -> ab <<< xa)

type Optic p s t a b
  = p a b -> p s t

type Lens s t a b
  = forall p. Strong p => Optic p s t a b

type Prism s t a b
  = forall p. Choice p => Optic p s t a b

type Grate s t a b
  = forall p. Closed p => Optic p s t a b

traversal :: forall a b p t. Wander p => Traversable t => p a b -> p (t a) (t b)
traversal = wander traverse

lens :: forall s t a b. (s -> Tuple a (b -> t)) -> Lens s t a b
lens f pab = dimap f (\(Tuple b x) -> x b) (first pab)

grate :: forall s t a b. (((s -> a) -> b) -> t) -> Grate s t a b
grate f pab = dimap (\s sa -> sa s) f (close pab)

prism :: forall s t a b. (b -> t) -> (s -> Either t a) -> Prism s t a b
prism fro to pab = dimap to (newFro fro) (right pab)
  where
  newFro :: (b -> t) -> Either t b -> t
  newFro f (Left t) = t

  newFro f (Right b) = f b

_1 = lens (\(Tuple x y) -> Tuple x (\i -> Tuple i y))

_2 = lens (\(Tuple x y) -> Tuple y (\i -> Tuple x i))

_Just =
  prism Just
    ( \x -> case x of
        Just y -> Right y
        Nothing -> Left Nothing
    )

iso = dimap

myGreatGrate :: Grate (Tuple Int Int) (Tuple Int Int) Int Int
myGreatGrate = grate (\f -> Tuple (f fst) (f snd))

main :: Effect Unit
main = do
  log ((dimap readFloat show ((+) 1.0)) "3.00")
  log (show ((_2 <<< _1) ((+) 55) (Tuple 1 (Tuple 101 2))))
  log (show ((_2 <<< _1 <<< _Just) ((+) 55) (Tuple 1 (Tuple Nothing 2))))
  log (show ((_2 <<< _1 <<< _Just) ((+) 55) (Tuple 1 (Tuple (Just 156) 2))))
  log (show ((_2 <<< _1 <<< traversal <<< _Just) ((+) 55) (Tuple 1 (Tuple [ (Just 156), (Just 1) ] 2))))
