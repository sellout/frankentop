-- Due to "Graphics.Implicit" (but it becomes Safe-Inferred with v0.4.1.0).
{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- Laptop chassis generator using the `implicit` library.
--
-- Mesh resolution is the first argument to writeSTL (mm). 0.5 gives a fine
-- mesh; 1.0–2.0 is faster for previewing.
module Main
  ( main,
  )
where

import safe "base" Data.Function (($))
import safe "base" Data.Semigroup ((<>))
import safe "base" System.IO (IO, putStrLn)
import safe "base" Text.Show (show)
import safe "frankentop-chassis" Frankentop.Chassis qualified as Chassis
import "implicit" Graphics.Implicit (writeSTL)
import safe "base" Prelude ((+))

-- | Generate STL files for the laptop chassis.
--
-- @since 0.0.1
main :: IO ()
main = do
  putStrLn $
    "dimensions: "
      <> show Chassis.width
      <> "×"
      <> show Chassis.depth
      <> "×"
      <> show (Chassis.baseHeight + Chassis.lidHeight)
      <> " mm"
  let res = 0.5 -- mesh resolution in mm; raise to 1–2 for faster preview
  writeSTL res "chassis.stl" Chassis.printableLayout
  putStrLn "Written: chassis.stl"
