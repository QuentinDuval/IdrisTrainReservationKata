module Occupancy

%access public export

record OccupancyRatio where
  constructor MkOccupancyRatio
  occupied : Nat
  seatCount : Nat

occupancyPercent : OccupancyRatio -> Double
occupancyPercent r =
  if occupied r >= seatCount r
    then 1.0
    else cast (occupied r) / cast (seatCount r)

Show OccupancyRatio where
  show r = "(" ++ show (occupied r) ++ ", " ++ show (seatCount r) ++ ")"

Semigroup OccupancyRatio where
  (<+>) a b = MkOccupancyRatio (occupied a + occupied b) (seatCount a + seatCount b)

Monoid OccupancyRatio where
  neutral = MkOccupancyRatio 0 0

Eq OccupancyRatio where
  r1 == r2 = occupancyPercent r1 == occupancyPercent r2

Ord OccupancyRatio where
  compare r1 r2 = compare (occupancyPercent r1) (occupancyPercent r2)

Cast OccupancyRatio Double where
  cast = occupancyPercent
