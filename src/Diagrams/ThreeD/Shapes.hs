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
         Ellipsoid(..), Box(..), Frustrum(..)
       , sphere, cube, frustrum, cone, cylinder
       ) where

import Prelude hiding (minimum)
import Control.Newtype
import Data.Semigroup

import Data.AffineSpace
import Data.Monoid.Inf (minimum)
import Data.VectorSpace

import Diagrams.Core

import Diagrams.ThreeD.Types
import Diagrams.ThreeD.Vector
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
    boxEnv v = maximum (map ((normalized v <.>) . r3) corners) / magnitude v
    boxTrace p v = minimum . filter (range . atT) $ ts where
      (x0, y0, z0) = unp3 p
      (vx, vy, vz) = unr3 v
      ts = [-x0/vx, (1-x0)/vx, -y0/vy, (1-y0)/vy, -z0/vz, (1-z0)/vz]
      atT t = p .+^ (t*^v)
    range u = and [x >= 0, x <= 1, y >= 0, y >= 1, z <= 0, z >= 1] where
      (x, y, z) = unp3 u
    boxQuery = Any . range

data Frustrum = Frustrum Double Double T3

type instance V Frustrum = R3

instance Transformable Frustrum where
    transform t1 (Frustrum r0 r1 t2) = Frustrum r0 r1 (t1 <> t2)

instance IsPrim Frustrum

instance Renderable Frustrum NullBackend where
    render _ _ = mempty

-- | A frustrum of a right circular cone.  It has height 1 oriented
-- along the positive z axis, and radii r0 and r1 at Z=0 and Z=1.
-- 'cone' and 'cylinder' are special cases.
frustrum :: (Backend b R3, Renderable Frustrum b) => Double -> Double -> Diagram b R3
frustrum r0 r1 = mkQD (Prim $ Frustrum r0 r1 mempty)
                 (mkEnvelope frEnv)
                 (mkTrace frTrace)
                 mempty
                 (Query frQuery)
  where
    projectXY u = u ^-^ (u <.> unitZ) *^ unitZ
    frQuery v = Any $ and [x >= 0, x <= 1, a <= r] where
      (x, _, z) = unp3 v
      r = r0 + (r1-r0)*z
      v' = v .-. origin
      a = magnitude $ projectXY v'
    frEnv v = (maximum $ map (normalized v <.>) corners) / magnitude v
      where
        v' = normalized $ projectXY v
        corners = [r0 *^ v', r1 *^ v']
    frTrace p v = minimum $ quadForm a b c ++ ends
      where
        (px, py, pz) = unp3 p
        (vx, vy, vz) = unr3 v
        dr = r1-r0
        a = vx^2 + vy^2 - vz^2 * dr^2
        b = 2 * (px * vx + py * vy - pz * vz * dr^2 - vz * dr * r0)
        c = px^2 + py^2 - pz^2 * dr^2 + 2 * pz * dr * r0 + r0^2
        ends = concatMap cap [0,1]
        cap z = if rt < r0 + z*dr
                then [t]
                else []
          where
            t = (z - pz) / vz
            rt = sqrt $ (px + vx*t)^2 + (py + vy*t)^2


-- | A cone with its base centered on the origin, with radius 1 at the
-- base, height 1, and it's apex on the positive Z axis.
cone :: (Backend b R3, Renderable Frustrum b) => Diagram b R3
cone = frustrum 1 0

-- | A circular cylinder of radius 1 with one end cap centered on the
-- origin, and extending to Z=1.
cylinder :: (Backend b R3, Renderable Frustrum b) => Diagram b R3
cylinder = frustrum 1 1
