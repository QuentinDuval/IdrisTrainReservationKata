module Occupancy

%access public export


record OccupancyRatio where
  constructor MkOccupancyRatio
  occupied : Nat
  seatCount : Nat

Show OccupancyRatio where
  show r = "(" ++ show (occupied r) ++ ", " ++ show (seatCount r) ++ ")"

Semigroup OccupancyRatio where
  (<+>) a b = MkOccupancyRatio (occupied a + occupied b) (seatCount a + seatCount b)

Monoid OccupancyRatio where
  neutral = MkOccupancyRatio 0 0

occupancyPercent : OccupancyRatio -> Double
occupancyPercent r =
  if occupied r >= seatCount r
    then 1.0
    else cast (occupied r) / cast (seatCount r)

Eq OccupancyRatio where
  r1 == r2 = occupancyPercent r1 == occupancyPercent r2

belowThreshold : Double -> OccupancyRatio -> Bool
belowThreshold threshold ratio = occupancyPercent ratio <= threshold

addOccupied : Nat -> OccupancyRatio -> OccupancyRatio
addOccupied seatRequest r = record { occupied $= (+ seatRequest) } r
