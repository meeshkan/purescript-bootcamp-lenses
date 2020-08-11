module NewMain where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Grate', Iso', _1, _2, _Just, iso, lens, over, preview, prism', set, toListOf, traversed, view, zipWithOf)
import Data.Lens.Common (simple)
import Data.Lens.Grate (grate)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.List as List
import Data.List.Lazy.NonEmpty (toList)
import Data.Map (singleton)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)

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

  Use of lenses [Optics]
  -------------
  Lens
  Prism
  Traversal
  Iso
  Grate

  Result
  ------
  Fold (Getter)
  Setter
-}
nested = Tuple 1 (Tuple 2 (Tuple 3 4))

lenz = _My2 <<< _My2 <<< _1

newtype Person
  = Person
  { person :: String
  , address ::
      Maybe
        { city :: String
        , status :: Status
        }
  }

derive instance genericPerson :: Generic Person _

derive instance newtypePerson :: Newtype Person _

instance showPerson :: Show Person where
  show p = genericShow p

data Status
  = Admin
  | Member

derive instance genericStatus :: Generic Status _

instance showStatus :: Show Status where
  show p = genericShow p

moreNested =
  Person
    { person: "Mike"
    , address:
        Just
          { city:
              "Helsinki"
          , status: Member
          }
    }

newNested =
  Person
    { person: "Makenna"
    , address: Nothing
    }

oneMore =
  Person
    { person: "Filipe"
    , address:
        Just
          { city:
              "Miami"
          , status: Member
          }
    }

-- data Maybe a = Just a | Nothing
lenz2 =
  simple _Newtype
    <<< prop (SProxy :: SProxy "address")
    <<< _MyOwnJust -- _Just
    <<< prop (SProxy :: SProxy "city")

lenz3 =
  simple _Newtype
    <<< prop (SProxy :: SProxy "address")
    <<< _MyOwnJust -- _Just
    <<< prop (SProxy :: SProxy "status")

_MyOwnJust = prism' Just identity

_MyMemberPrism =
  prism' identity
    ( \x -> case x of
        Member -> Just Member
        _ -> Nothing
    )

_My2 = lens snd (\(Tuple a _) v -> Tuple a v)

lensWithIso =
  iso List.fromFoldable Map.fromFoldable
    <<< traversed
    <<< _2
    <<< lenz2

myGreatGrate :: Grate' (Tuple Int Int) Int
myGreatGrate = grate (\f -> Tuple (f fst) (f snd))

-- mapToListIso :: forall a. Iso' (Map.Map String a) (List (Tuple String a))
-- mapToListIso = iso Map.toUnfoldable List.toUnfoldable
main :: Effect Unit
main = do
  log (show (set lenz 42 nested))
  log (show (view lenz nested))
  log (show (set lenz2 "Tampere" moreNested))
  log (show (over lenz2 (append "Suburbs of ") moreNested))
  log (show (preview (lenz3 <<< _MyMemberPrism) moreNested))
  log (show (preview lenz2 newNested))
  log (show (over lenz2 (append "Suburbs of ") newNested))
  log
    ( show
        ( over
            ( (prop (SProxy :: SProxy "a"))
                <<< traversed
                <<< lenz2
            )
            (append "Suburbs of ")
            { a: [ moreNested, newNested, oneMore ] }
        )
    )
  log
    ( show
        ( toListOf
            ( (prop (SProxy :: SProxy "a"))
                <<< traversed
                <<< lenz2
            )
            { a: [ moreNested, newNested, oneMore ] }
        )
    )
  log (show (zipWithOf myGreatGrate (-) (Tuple 3 4) (Tuple 5 6)))

{-
   log
    ( show
        ( preview
            ( iso Map.toUnfoldable Map.fromFoldable
                <<< traversed
            )
            (singleton "hello" moreNested)
        )
    )
  -}
