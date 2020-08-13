module Cursor exposing
  ( Cursor(..)
  , fromList
  , reverse
  , current
  , update
  , forward
  , back
  )

type Cursor a = Cursor (List a) a (List a)

fromList : a -> List a -> Cursor a
fromList = Cursor []

reverse : Cursor a -> Cursor a
reverse (Cursor before x after) = Cursor after x before

current : Cursor a -> a
current (Cursor _ x _) = x

update : a -> Cursor a -> Cursor a
update new (Cursor before _ after) = Cursor before new after

forward : Cursor a -> Maybe (Cursor a)
forward (Cursor before x after) =
  case after of
    [] -> Nothing
    y :: ys -> Just <| Cursor (x :: before) y ys

back : Cursor a -> Maybe (Cursor a)
back (Cursor before x after) =
  case before of
    [] -> Nothing
    y :: ys -> Just <| Cursor ys y (x :: after)
