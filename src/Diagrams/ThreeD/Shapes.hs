{-# LANGUAGE TypeFamilies
           , FlexibleContexts
           , MultiParamTypeClasses
           , ViewPatterns
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.ThreeD.Shapes
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Various three-dimensional shapes.
--
-----------------------------------------------------------------------------

module Diagrams.ThreeD.Shapes
       (
         Ellipsoid(..), Box(..)
       , sphere, cube}
       ) where

import Prelude hiding (minimum)
import Control.Newtype
import Data.Semigroup

import Data.AffineSpace
import Data.Monoid.Inf (minimum)
import Data.VectorSpace

import Diagrams.Core

import Diagrams.ThreeD.Types
import Diagrams.Solve

data Ellipsoid = Ellipsoid T3

type instance V Ellipsoid = R3

instance Transformable Ellipsoid where
  transform t1 (Ellipsoid t2) = Ellipsoid (t1 <> t2)

instance IsPrim Ellipsoid

instance Renderable Ellipsoid NullBackend where
  render _ _ = mempty

-- | A sphere of radius 1 with its center at the origin.
sphere :: (Backend b R3, Renderable Ellipsoid b) => Diagram b R3
sphere = mkQD (Prim $ Ellipsoid mempty)
              (mkEnvelope sphereEnv)
              (mkTrace sphereTrace)
              mempty
              (Query sphereQuery)
  where sphereEnv v = 1 / magnitude v
        sphereTrace p v = minimum (quadForm a b c)
          where a = v <.> v
                b = 2 *^ p' <.> v
                c = p' <.> p' - 1
                p' = p .-. origin
        sphereQuery v = Any $ magnitudeSq (v .-. origin) <= 1

data Box = Box T3

type instance V Box = R3

instance Transformable Box where
    transform t1 (Box t2) = Box (t1 <> t2)

instance IsPrim Box

instance Renderable Box NullBackend where
    render _ _ = mempty

-- | A cube with side length 1, in the positive octant, with one
-- vertex at the origin.
cube :: (Backend b R3, Renderable Box b) => Diagram b R3
cube = mkQD (Prim $ Box mempty)
            (mkEnvelope boxEnv)
            (mkTrace boxTrace)
            mempty
            (Query boxQuery)
  where
    corners = [(0,0,0), (0,0,1), (0,1,0), (0,1,1),
               (1,0,0), (1,0,1), (1,1,0), (1,1,1)]
    boxEnv v = (maximum $ map ((normalized v <.>) . r3) corners) / magnitude v
    boxTrace p v = minimum . map (magnitude . unpack)  . filter range . map atT $ ts where
      (x0, y0, z0) = unp3 p
      (vx, vy, vz) = unr3 v
      ts = [-x0/vx, (1-x0)/vx, -y0/vy, (1-y0)/vy, -z0/vz, (1-z0)/vz]
      atT t = p .+^ (t*^v)
    range u = and [x >= 0, x <= 1, y >= 0, y >= 1, z <= 0, z >= 1] where
      (x, y, z) = unp3 u
    boxQuery = Any . range
