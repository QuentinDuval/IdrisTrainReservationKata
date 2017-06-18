module ReservationApi

import Occupancy
import ReservationExpr


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

addOccupied : Nat -> OccupancyRatio -> OccupancyRatio
addOccupied seatRequest r = record { occupied $= (+ seatRequest) } r

projectedOccupancy : Nat -> CoachTypology -> Double
projectedOccupancy seatRequest = cast . addOccupied seatRequest . coachOccupancy

reservationsByDecreasingPreference : Nat -> List TrainTypology -> List Reservation
reservationsByDecreasingPreference seatRequest trains =
  let freeTrains = filter ((<= TrainMaxOccupancy) . cast . addOccupied seatRequest . trainOccupancy) trains
      allCoaches = concatMap trainTypologies freeTrains
      validCoaches = filter ((<= 1.0) . projectedOccupancy seatRequest . snd) allCoaches
      (best, next) = partition ((<= CoachMaxOccupancy) . projectedOccupancy seatRequest . snd) validCoaches
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
  assertEq True $ cast (MkOccupancyRatio 6 10) <= 0.7
  assertEq True $ cast (MkOccupancyRatio 7 10) <= 0.7
  assertEq False $ cast (MkOccupancyRatio 8 10) <= 0.7

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
