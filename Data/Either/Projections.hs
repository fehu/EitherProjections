-----------------------------------------------------------------------------
--
-- Module      :  Data.Either.Projections
-- License     :  MIT
-- Portability :  GHC
--
-- | Either Projections, inspired by
--   <http://www.scala-lang.org/api/rc2/scala/Either.html Scala's Either>.
--
-- Example:
--
-- >>> let process = (+) 100 . (4 *)
-- >>> let foo = fmap (10 *) . rightProjection
--
-- >>> let ok = Right 10 :: Either String Int
-- >>> let fail = Left "wrong input" :: Either String Int
--
-- >>> foo ok
-- RightProjection 140
--
-- >>> foo fail
-- RightNothing "wrong input"
--
-- >>> toMaybe $ foo fail
-- Nothing
--
-- >>> toMaybe $ foo ok
-- Just 140
--
-- >>> toEither $ foo fail
-- Left "wrong input"
--
-- >>> mergeEither . toEither . fmap show $ rightProjection ok
-- "10"


{-# OPTIONS_HADDOCK show-extensions #-}

module Data.Either.Projections (

-- * Projections.

  EitherProjection (toEither, toMaybe)

, LeftProjection, LeftProjection'
, RightProjection

, leftProjection, rightProjection

-- * misc

, mapLeft, mapRight

, mergeEither

) where

import Data.Typeable



-- | A projection of 'Either'.
class EitherProjection proj left right side | proj -> left
                                            , proj -> right
                                            , proj -> side
    where toEither :: proj -> Either left right
          toMaybe  :: proj -> Maybe side


-- | 'Left' projection.
type LeftProjection l r = LeftProjection' r l

-- | 'Left' projection with type arguments flipped.
-- | Allows to define instances of 'Functor', etc.
data LeftProjection' r l = LeftProjection l | LeftNothing r
    deriving (Eq, Ord, Read, Show, Typeable)

-- | 'Right' projection.
data RightProjection l r = RightProjection r | RightNothing l
    deriving (Eq, Ord, Read, Show, Typeable)

-----------------------------------------------------------------------------

-- | Get left projection.
leftProjection :: Either l r -> LeftProjection l r
leftProjection x = case x of Left  l -> LeftProjection l
                             Right r -> LeftNothing r

-- | Get right projection.
rightProjection  :: Either l r -> RightProjection l r
rightProjection x = case x of Right r -> RightProjection r
                              Left  l -> RightNothing l

-----------------------------------------------------------------------------

mapLeft :: (l -> t) ->  Either l r -> Either t r
mapLeft f (Left l)  = Left (f l)
mapLeft _ (Right r) = Right r

mapRight :: (r -> t) ->  Either l r -> Either l t
mapRight f (Right r) = Right (f r)
mapRight _ (Left l)  = Left l

-----------------------------------------------------------------------------


instance EitherProjection (LeftProjection l r) l r l
    where toEither (LeftProjection l) = Left l
          toEither (LeftNothing r)    = Right r
          toMaybe  (LeftProjection l) = Just l
          toMaybe  _                  = Nothing

instance EitherProjection (RightProjection l r) l r r
    where toEither (RightProjection r) = Right r
          toEither (RightNothing l)    = Left l
          toMaybe  (RightProjection r) = Just r
          toMaybe  _                  = Nothing



instance Functor (LeftProjection' r) where
    fmap f (LeftProjection l) = LeftProjection (f l)
    fmap _ (LeftNothing    r) = LeftNothing r

instance Functor (RightProjection l) where
    fmap f (RightProjection r) = RightProjection (f r)
    fmap _ (RightNothing    l)  = RightNothing l


-----------------------------------------------------------------------------

-- | Merge 'Left' and 'Right'. Inspired by
--   <http://www.scala-lang.org/api/rc2/scala/Either$$MergeableEither.html Scala>.
mergeEither :: Either a a -> a
mergeEither x = case x of Left  l -> l
                          Right r -> r

