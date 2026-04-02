-- Due to "Graphics.Implicit" (but it becomes Safe-Inferred with v0.4.1.0).
{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- Laptop chassis generator using the `implicit` library.
module Frankentop.Chassis
  ( width,
    depth,
    baseHeight,
    lidHeight,
    base,
    lid,
    fullyOpen,
    open,
    closed,
    printableLayout,
  )
where

import safe "base" Control.Category ((.))
import safe "base" Data.Bool (Bool (False, True))
import safe "base" Data.Function (($))
import safe "base" Data.Functor (fmap)
import safe "base" Data.Int (Int)
-- FIXME: Use a `Vec`.
import safe "base" Data.List ((!!))
import "implicit" Graphics.Implicit
  ( SymbolicObj2,
    SymbolicObj3,
    cube,
    cylinder,
    difference,
    extrude,
    mirror,
    polygon,
    rotate3,
    square,
    translate,
    union,
    withRounding,
    ℝ,
    ℝ3,
  )
import safe "linear" Linear.V2 (V2 (V2))
import safe "linear" Linear.V3 (V3 (V3))
import safe "base" Prelude (max, negate, pi, (*), (+), (-), (/))

-- Shared footprint
cornerRadius, width, depth, margin, cableChannel, thinMargin :: ℝ
cornerRadius = margin

-- | Overall width of the chassis (common to `base` & `lid`).
--
-- @since 0.0.1
width =
  max (screenWidth + 2 * margin) $
    max atreusWidth cometWidth + 2 * margin

-- | Overall depth of the chassis (common to `base` & `lid`)..
--
-- @since 0.0.1
depth =
  max (screenDepth - thickScreenDepth + margin) $
    atreusDepth + cometDepth + 2 * (cableChannel + thinMargin)

margin = 2 -- edge margin, floor thickness (both pieces) – see https://bigrep.com/posts/designing-wall-thickness-for-3d-printing/

cableChannel = 5 -- width needed to run a cable

thinMargin = 1.2 -- acceptable when it’s only a small part of a full margin

-- ── Base plate ────────────────────────────────────────────────────────────────

-- | Thickness of the laptop base.
--
-- @since 0.0.1
baseHeight :: ℝ
baseHeight = cometHeight + margin -- 17

-- Keyboardio Atreus: 243×100 mm body, 10° inward taper per half
atreusWidth, atreusRearWidth, atreusDepth, aRx0, aRx1, aRy0, aRyO, aRy1, aFlX, aFliX, aFriX, aFrX :: ℝ
atreusWidth = 243
atreusRearWidth = 215
atreusDepth = 100
aRx0 = (width - atreusRearWidth) / 2 -- 36
aRx1 = aRx0 + atreusRearWidth -- 279
aRy1 = depth - (cableChannel + thinMargin) -- 185
aRyO = (aRy1 - atreusDepth) + 20 -- 85
aRy0 = aRy1 - atreusDepth -- 85
aFlX = (width - atreusWidth) / 2 -- 36
aFliX = (width - 55) / 2 -- 36
aFriX = (width + 55) / 2 -- 36 -- 279
aFrX = aFlX + atreusWidth -- 279

-- |
--
--  __TODO__: This is drawn in the correct place on the plane, but we should
--            really just translate it later.
atreusArea :: SymbolicObj2
atreusArea =
  polygon
    [ V2 aFriX aRy0, -- right junction → Atreus front-right
      V2 aFrX aRyO,
      V2 aRx1 aRy1,
      V2 aRx0 aRy1, -- Atreus rear edge
      V2 aFlX aRyO,
      V2 aFliX aRy0
    ]

atreusLeftKeys :: SymbolicObj2
atreusLeftKeys =
  polygon
    [ V2 aRx0 aRy1,
      V2 (aRx0 + 60) aRy1,
      V2 (aRx0 + 95) $ aRy1 - 15,
      V2 (aRx0 + 90) $ aRy1 - 55, -- not part of convex hull
      V2 ((width / 2) - 1.5) $ aRy1 - 60, -- narrow edge
      V2 ((width / 2) - 5) aRy0,
      V2 aFliX aRy0, -- wide edge
      V2 aFlX aRyO -- wide edge
    ]

atreusKeys :: SymbolicObj3
atreusKeys =
  extrude
    ( union
        [ atreusLeftKeys,
          translate (V2 width 0) $ mirror (V2 1 0) atreusLeftKeys
        ]
    )
    18

atreusBody :: SymbolicObj3
atreusBody = extrude atreusArea 10

-- Atreus keyboard through-cutout polygon (CCW, lid absolute coords).

leftCutPoly :: SymbolicObj2
leftCutPoly =
  polygon
    [ V2 aRx0 aLy0,
      V2 (aRx0 + 60) aLy0,
      V2 (aRx0 + 95) $ aLy0 + 15,
      V2 ((width / 2) - 1.5) $ aLy0 + 60, -- narrow edge
      V2 ((width / 2) - 5) aLy1,
      V2 aFliX aLy1, -- wide edge
      V2 aFlX aLyO -- wide edge
    ]

rightCutPoly :: SymbolicObj2
rightCutPoly = translate (V2 width 0) $ mirror (V2 1 0) leftCutPoly

atreus :: SymbolicObj3
atreus = union [atreusBody, translate (V3 0 0 10) atreusKeys]

-- Mecha Comet: 155×73 mm body
cometWidth, cometDepth, cometHeight, cometScreenOffset, cRx0, cRy0, cRy1 :: ℝ
cometWidth = 155
cometDepth = 73
cometHeight = 14
-- FIXME: This is just a guess for now.
cometScreenOffset = 20 -- distance from the center of the screen to the center of the comet
cRx0 = (width - cometWidth) / 2 + cometScreenOffset -- 80
cRy1 = aRy0 -- 85  (abuts Atreus front — no wall between pockets)
cRy0 = cRy1 - cometDepth -- 12

comet :: SymbolicObj3
comet = cube False $ V3 cometWidth cometDepth cometHeight

cometPosition :: ℝ3
cometPosition = V3 cRx0 cRy0 margin

-- ── Lid ───────────────────────────────────────────────────────────────────────
-- UPERFECT 13.3" OLED display: step at 65 mm from top edge.
--   Thick portion (8 mm, y=120..185): rests on lid surface, no pocket.
--   Thin portion (4 mm, y= 65..190): goes into 4 mm receptacle, open at free edge.
screenWidth, screenDepth, thickScreenDepth, screenHeight, lidHeight, sSx0, sSy0, sSy1, sPf :: ℝ
screenWidth = 305
screenDepth = 125
thickScreenDepth = 65
screenHeight = 4

-- | Thickness of the laptop lid.
--
-- @since 0.0.1
lidHeight = screenHeight + margin -- 7

sSx0 = (width - screenWidth) / 2 -- 7.5
-- sSx1 = sSx0 + screenWidth -- 307.5

sSy1 = depth -- 190  (receptacle aligned with free/display edge)

sSy0 = sSy1 - screenDepth -- 65

sPf = lidHeight - screenHeight -- 3   (pocket floor z)

screen :: SymbolicObj3
screen =
  union
    [ -- rect3 (V3 0 screenDepth -4) $ V3 screenWidth thickScreenDepth screenHeight,
      translate (V3 0 screenDepth -4) . cube False $ V3 screenWidth thickScreenDepth 8,
      cube False $ V3 screenWidth screenDepth screenHeight
    ]

-- ── Hinge knuckles: 3-knuckle interleave, M6-compatible bore ─────────────────
knuckleOuterRadius, knuckleInnerRadius :: ℝ
knuckleOuterRadius = knuckleInnerRadius + 3
knuckleInnerRadius = 1.7

-- knuckle edges – one half should start even, end odd; the other start odd end
-- even
knuckleEdges :: [ℝ]
knuckleEdges = [8, 26, width / 2 - 9, width / 2 + 9, width - 26, width - 8]

knuckleDepthOffset :: ℝ
knuckleDepthOffset = knuckleInnerRadius

-- ── Bottom feet ───────────────────────────────────────────────────────────────
footRadius, footHeight :: ℝ
footRadius = 4
footHeight = 2

footPosition :: [(ℝ, ℝ)]
footPosition = [(20, 20), (width - 20, 20), (20, depth - 20), (width - 20, depth - 20)]

-- ── Keyboard cutout in lid (y-flipped to align when closed) ──────────────────
-- When the lid closes: base_y = d - lid_y
-- Atreus base y = [85..185]  ↔  lid y = [5..105]
--   Wide end (243 mm) at lid y = 5  → maps to base rear  (y=185)
--   Narrow end          at lid y = 105 → maps to base front (y=85)
aLy0, aLyO, aLy1 :: ℝ
aLy0 = depth - aRy1 -- 5
aLyO = depth - aRyO -- 5
aLy1 = depth - aRy0 -- 105

-- Hollow tube along the X axis, centred at (y=cy, z=cz), from x=x0 to x=x1.
--
-- Build a hollow cylinder along Z (outer minus bore), then rotate 90° around
-- Y (mapping Z→X), then translate into position.
--
-- After R_y(π/2): a point (x,y,z) → (z, y, −x), so the cylinder axis
-- (originally 0..len along Z) becomes 0..len along X.  Translating by
-- (x0, cy, cz) places the left face at (x0, cy, cz). ✓
knuckle :: ℝ -> ℝ -> ℝ -> ℝ -> SymbolicObj3
knuckle cy cz x0 x1 =
  let len = x1 - x0
      body =
        difference
          (cylinder knuckleOuterRadius len)
          [cylinder knuckleInnerRadius len]
      rotd = rotate3 (V3 0 (pi / 2) 0) body
   in translate (V3 x0 cy cz) rotd

port :: Bool -> ℝ -> SymbolicObj3
port rightSide cy =
  let portDepth = 9
      portHeight = 3.3
   in translate
        ( V3 (if rightSide then width - 8 else -1) (cy - (portDepth / 2)) $
            (baseHeight - portHeight) / 2
        )
        . cube False
        $ V3 9 portDepth portHeight

profile :: SymbolicObj2
profile = withRounding cornerRadius . square False $ V2 width depth

knuckles :: ℝ -> ℝ -> [Int] -> SymbolicObj3
knuckles cy cz =
  union
    . fmap
      ( \start ->
          knuckle cy cz (knuckleEdges !! start) $ knuckleEdges !! (start + 1)
      )

-- | Complete model of the laptop base.
--
-- @since 0.0.1
base :: SymbolicObj3
base =
  union
    [ -- Chamfered shell with features subtracted
      difference
        (extrude profile baseHeight)
        [ -- Combined Atreus+Comet pocket (depth 14 mm, floor at z=3)
          translate (V3 0 0 margin) $ extrude atreusArea cometHeight,
          translate cometPosition comet,
          -- Comet USB-C notch: 12×6 mm indicator on right outer wall
          port True $ depth - 24, -- display
          port True $ depth - 48, -- power
          port True $ depth - 72, -- accessories
          port False $ depth - 72,
          knuckles (depth + knuckleDepthOffset) baseHeight [1, 3]
        ],
      -- Bottom feet (cylinders hanging below z=0)
      union
        [ translate (V3 fx fy (-footHeight)) (cylinder footRadius footHeight)
        | (fx, fy) <- footPosition
        ],
      -- Two hinge knuckles on base rear edge, outside Atreus x-range
      knuckles (depth + knuckleDepthOffset) baseHeight [0, 2, 4],
      -- optional bits
      translate (V3 0 0 margin) atreus,
      translate cometPosition comet
    ]

-- | Complete model of the laptop lid.
--
-- @since 0.0.1
lid :: SymbolicObj3
lid =
  let -- Screen receptacle: 300×125×4 mm, open at free edge
      positionedScreen = translate (V3 sSx0 sSy0 sPf) screen
      _lowScreen = translate (V3 sSx0 (knuckleOuterRadius - knuckleInnerRadius) 7) screen
   in union
        [ -- Chamfered shell with features subtracted
          difference
            (extrude profile lidHeight)
            [ positionedScreen,
              -- Keyboard cutout: full-thickness trapezoid hole aligned with base pocket
              extrude leftCutPoly lidHeight,
              extrude rightCutPoly lidHeight,
              knuckles (negate knuckleDepthOffset) lidHeight [0, 2, 4]
            ],
          -- One hinge knuckle on lid hinge edge, centred to interleave with base pair
          knuckles (negate knuckleDepthOffset) lidHeight [1, 3],
          -- optional bits
          positionedScreen
          -- _lowScreen
        ]

-- | Complete model of the laptop, open 0.5τ.
--
-- @since 0.0.1
fullyOpen :: SymbolicObj3
fullyOpen = open pi

-- | Complete model of the laptop, closed.
--
-- @since 0.0.1
closed :: SymbolicObj3
closed = open 0

-- | Complete model of the laptop, open.
--
-- @since 0.0.1
open ::
  -- | How many radians open the laptop is. `0` is closed, `pi` is opened flat.
  ℝ ->
  SymbolicObj3
open radians =
  let lidTranslation = V3 0 knuckleDepthOffset -lidHeight
      rotatedLid =
        translate (negate lidTranslation) . rotate3 (V3 (pi - radians) 0 0) $
          translate lidTranslation lid
   in union
        [ base,
          translate
            (V3 0 (depth + knuckleInnerRadius * 2) (baseHeight - lidHeight))
            rotatedLid
        ]

-- | Complete model of the laptop, suitable for 3D printing.
--
-- @since 0.0.1
printableLayout :: SymbolicObj3
printableLayout = union [base, translate (V3 0 (depth + 20) 0) lid]
