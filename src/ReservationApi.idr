module ReservationApi

import ReservationExpr


--------------------------------------------------------------------------------
-- The code (should follow the rule of the DSL)
-- Decoupling is pretty good:
-- * Invariants of the Business Rules are in the DSL
-- * Implementation of the SPI are in the interpreter
-- * Current implementation that satisfies the rules is exression of the DSL
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- RULES:
-- TODO: sum the total seats to check that will not go over 70% of the train
-- TODO: check the typology of the coaches to put the same family in one coach
-- TODO: ideally, we should not go over 70% in one coach
--------------------------------------------------------------------------------

TrainMaxOccupancy : Double
TrainMaxOccupancy = 0.7

CoachMaxOccupancy : Double
CoachMaxOccupancy = 0.8

coachOccupancy : CoachTypology -> OccupancyRatio
coachOccupancy coach =
  let totalSeats = totalSeatCount coach
      freeSeats = length (availableSeats coach)
  in case isLTE freeSeats totalSeats of
        Yes prf => MkOccupancyRatio (totalSeats - freeSeats) totalSeats
        No contra => MkOccupancyRatio totalSeats totalSeats

trainOccupancy : TrainTypology -> OccupancyRatio
trainOccupancy train = concatMap coachOccupancy (coaches train)

trainTypologies : TrainTypology -> List (TrainId, CoachTypology)
trainTypologies train = map (\coach => (trainId train, coach)) (coaches train)

coachToReservation : Nat -> (TrainId, CoachTypology) -> Reservation
coachToReservation seatRequest (trainId, coach) =
  MkReservation trainId (coachId coach) (take seatRequest (availableSeats coach))

projectedOccupancy : Nat -> CoachTypology -> OccupancyRatio -- TODO: memoize
projectedOccupancy seatRequest = addOccupied seatRequest . coachOccupancy

reservationsByDecreasingPreference : Nat -> List TrainTypology -> List Reservation
reservationsByDecreasingPreference seatRequest trains =
  let freeTrains = filter (belowThreshold TrainMaxOccupancy . addOccupied seatRequest . trainOccupancy) trains
      allCoaches = concatMap trainTypologies freeTrains
      validCoaches = filter (belowThreshold 1.0 . projectedOccupancy seatRequest . snd) allCoaches
      (best, next) = partition (belowThreshold CoachMaxOccupancy . projectedOccupancy seatRequest . snd) validCoaches
  in map (coachToReservation seatRequest) (best ++ next)

export
reserve : ReservationRequest -> ReservationExpr ReservationResult
reserve request = do
    trainIds <- SearchTrain (dateTime request)
    typologies <- sequence (map GetTypology trainIds)
    let reservations = reservationsByDecreasingPreference (seatCount request) typologies
    Log (show reservations)
    confirmByPref reservations
  where
    confirmByPref [] = Pure NoTrainAvailable
    confirmByPref (r::rs) = do
      validated <- Reserve r
      case validated of
        Nothing => confirmByPref rs
        Just ok => Pure (Confirmed ok)


--------------------------------------------------------------------------------
-- Run Tests
--------------------------------------------------------------------------------

assertEq : (Eq a, Show a) => (given : a) -> (expected : a) -> IO ()
assertEq g e = if g == e
  then putStrLn $ "Test Passed!"
  else putStrLn $ "Test Failed: " ++ show g ++ " /= " ++ show e

occupancy_ratio_test : IO ()
occupancy_ratio_test = do
  assertEq True $ belowThreshold 0.7 (MkOccupancyRatio 6 10)
  assertEq True $ belowThreshold 0.7 (MkOccupancyRatio 7 10)
  assertEq False $ belowThreshold 0.7 (MkOccupancyRatio 8 10)

coach_occupancy_test : IO ()
coach_occupancy_test = do
  assertEq (MkOccupancyRatio 4 100) (coachOccupancy (MkCoachTypology "A" 100 [5..100]))
  assertEq (MkOccupancyRatio 8 200) (trainOccupancy (MkTrainTypology "T" [ MkCoachTypology "A" 100 [4..100]
                                                                         , MkCoachTypology "B" 100 [6..100] ]))

coach_reservation_test : IO ()
coach_reservation_test = do
  assertEq (MkReservation "T" "A" [5..14]) $ coachToReservation 10 ("T", MkCoachTypology "A" 100 [5..100])

run_tests : IO ()
run_tests = do
  occupancy_ratio_test
  coach_occupancy_test
  coach_reservation_test

--
