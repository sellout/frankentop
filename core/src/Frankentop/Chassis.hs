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
    -- mirror,
    polygon,
    rotate3,
    square,
    translate,
    union,
    withRounding,
    ℝ,
  )
import safe "linear" Linear.V2 (V2 (V2))
import safe "linear" Linear.V3 (V3 (V3))
import safe "base" Prelude (max, pi, (*), (+), (-), (/))

-- Shared footprint
cornerRadius, width, depth, margin, cableChannel, thinMargin :: ℝ
cornerRadius = margin

-- | Overall width of the chassis (common to `base` & `lid`).
--
-- @since 0.0.1
width =
  max (screenWidth + 2 * margin) $
    (max atreusWidth cometWidth) + 2 * margin

-- | Overall depth of the chassis (common to `base` & `lid`)..
--
-- @since 0.0.1
depth =
  max (screenDepth - thickScreenDepth + margin) $
    atreusDepth + cometDepth + 2 * (cableChannel + thinMargin)

margin = 3 -- edge margin, floor thickness (both pieces)

cableChannel = 5 -- width needed to run a cable

thinMargin = 1 -- acceptable when it’s only a small part of a full margin

-- ── Base plate ────────────────────────────────────────────────────────────────
cometHeight, baseHeight :: ℝ
cometHeight = 14 -- pocket depth = Comet device height

-- | Thickness of the laptop base.
--
-- @since 0.0.1
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

-- Mecha Comet: 155×73 mm body
cometWidth, cometDepth, cometScreenOffset, cRx0, cRx1, cRy0, cRy1 :: ℝ
cometWidth = 155
cometDepth = 73
-- FIXME: This is just a guess for now.
cometScreenOffset = 20 -- distance from the center of the screen to the center of the comet
cRx0 = (width - cometWidth) / 2 + cometScreenOffset -- 80
cRx1 = cRx0 + cometWidth + cometScreenOffset -- 235
cRy1 = aRy0 -- 85  (abuts Atreus front — no wall between pockets)
cRy0 = cRy1 - cometDepth -- 12

-- ── Lid ───────────────────────────────────────────────────────────────────────
-- UPERFECT 13.3" OLED display: step at 65 mm from top edge.
--   Thick portion (8 mm, y=120..185): rests on lid surface, no pocket.
--   Thin portion (4 mm, y= 65..190): goes into 4 mm receptacle, open at free edge.
screenWidth, screenDepth, thickScreenDepth, screenHeight, lidHeight, sSx0, sSy0, sSy1, sPf :: ℝ
screenWidth = 300
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

-- Combined Atreus+Comet pocket polygon (CCW, absolute base coords).
-- The Comet rect and Atreus trapezoid share an edge at y=85; no wall between.
comboPoly :: SymbolicObj2
comboPoly =
  polygon
    [ V2 cRx0 cRy0,
      V2 cRx1 cRy0, -- Comet front edge
      V2 cRx1 cRy1,
      V2 aFriX aRy0, -- right junction → Atreus front-right
      V2 aFrX aRyO,
      V2 aRx1 aRy1,
      V2 aRx0 aRy1, -- Atreus rear edge
      V2 aFlX aRyO,
      V2 aFliX aRy0,
      V2 cRx0 cRy1 -- Atreus front-left → left junction
    ]

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

-- |
--
--  __TODO__: Calcluate this by mirroring `leftCutPoly`.
rightCutPoly :: SymbolicObj2
-- rightCutPoly = mirror (V2 (width / 2) 0) leftCutPoly
rightCutPoly =
  polygon
    [ V2 (aRx1 - 60) aLy0,
      V2 aRx1 aLy0, -- narrow edge
      V2 aFrX aLyO,
      V2 aFriX aLy1,
      V2 ((width / 2) + 5) aLy1,
      V2 ((width / 2) + 1.5) $ aLy0 + 60, -- wide edge
      V2 (aRx1 - 95) $ aLy0 + 15
    ]

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
          translate (V3 0 0 margin) $ extrude comboPoly cometHeight,
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
      knuckles (depth + knuckleDepthOffset) baseHeight [0, 2, 4]
    ]

-- | Complete model of the laptop lid.
--
-- @since 0.0.1
lid :: SymbolicObj3
lid =
  union
    [ -- Chamfered shell with features subtracted
      difference
        (extrude profile lidHeight)
        [ -- Screen receptacle: 300×125×4 mm, open at free edge
          translate
            (V3 sSx0 sSy0 sPf)
            $ cube False (V3 screenWidth screenDepth screenHeight),
          -- Keyboard cutout: full-thickness trapezoid hole aligned with base pocket
          extrude leftCutPoly lidHeight,
          extrude rightCutPoly lidHeight,
          knuckles (0 - knuckleDepthOffset) lidHeight [0, 2, 4]
        ],
      -- One hinge knuckle on lid hinge edge, centred to interleave with base pair
      knuckles (0 - knuckleDepthOffset) lidHeight [1, 3]
    ]

-- | Complete model of the laptop, suitable for 3D printing.
--
-- @since 0.0.1
printableLayout :: SymbolicObj3
printableLayout = union [base, translate (V3 0 (depth + 20) 0) lid]
