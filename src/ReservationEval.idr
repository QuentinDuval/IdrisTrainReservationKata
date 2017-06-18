module ReservationEval

import ReservationExpr


--------------------------------------------------------------------------------
-- Interpreter: Transformation from the abstract problem to real world
--
-- Here, we implement a fake interpreter than for the example
--------------------------------------------------------------------------------

fakeTrainDB : List TrainTypology
fakeTrainDB =
  [ MkTrainTypology "T1" [MkCoachTypology "A" 100 [71..100], MkCoachTypology "B" 100 [71..100]] -- Full Train
  , MkTrainTypology "T2" [MkCoachTypology "A" 100 [80..100], MkCoachTypology "B" 100 [50..100]] -- First coach full
  , MkTrainTypology "T3" [MkCoachTypology "A" 100 [5..100]] -- Plenty of places
  ]

searchTrainAt : DateTime -> IO (List TrainId)
searchTrainAt _ = pure (map trainId fakeTrainDB)

getTypologyOf : TrainId -> IO (Maybe TrainTypology)
getTypologyOf tid = pure $ find ((== tid). trainId) fakeTrainDB

confirmCommand : Reservation -> IO (Maybe Reservation)
confirmCommand r = pure (Just r)

export
evalReservation : ReservationExpr ty -> IO ty
evalReservation (Log msg) = putStrLn msg
evalReservation (Pure val) = pure val
evalReservation (Bind val next) = evalReservation val >>= evalReservation . next
evalReservation (SearchTrain time) = searchTrainAt time
evalReservation (GetTypology trainId) = getTypologyOf trainId
evalReservation (Reserve command) = confirmCommand command
