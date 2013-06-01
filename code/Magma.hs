module Magma where

-- モノイドはマグマの一種なのでimportしときます。
import Data.Monoid
import Data.Foldable

-- マグマとは集合A(型aと対応)とA上の二項演算(magappendと対応)を
-- 備えた代数構造です。
class Magma a where
    magappend :: a -> a -> a

-- 自由マグマとは二分木です。
-- Nodeが自由マグマの積。
-- Leafが自由対象のスライドにあるiです。
data FreeMagma a = Leaf a | Node (FreeMagma a) (FreeMagma a) deriving (Show, Eq)

-- foldMapMagma fが自由対象のスライドにある「fバー」です。
foldMapMagma :: Magma b => (a -> b) -> FreeMagma a -> b
foldMapMagma f (Leaf x) = f x
foldMapMagma f (Node l r) = foldMapMagma f l `magappend` foldMapMagma f r

-- 実際にはData.Foldableのインスタンスにした方が良いでしょう。
-- foldr, foldl, find, elem, notElem, concat, forM_, ....などなどの便利関数が
-- 使える様になります。

-- 自由マグマはもちろんマグマです。
instance Magma (FreeMagma a) where
    magappend = Node

-- モノイドもマグマです。
newtype WrappedMonoid a = WrapMonoid a deriving (Show, Eq)
instance Monoid a => Magma (WrappedMonoid a) where
    WrapMonoid x `magappend` WrapMonoid y = WrapMonoid (x `mappend` y)

-- モノイドでないマグマ演算の例として
-- "Hello" `magappend` "World" == "(Hello World)"
-- みたいに括弧で囲むものを作ってみます。
newtype Kakko = Kakko String deriving (Show)
instance Magma Kakko where
    Kakko x `magappend` Kakko y = Kakko ("(" ++ x ++ " " ++ y ++ ")")


