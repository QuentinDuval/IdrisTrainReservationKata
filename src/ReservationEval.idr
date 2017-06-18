module ReservationEval

import ReservationExpr


--------------------------------------------------------------------------------
-- Interpreter: this is the transformation from the abstract problem
-- to the real world (allows to plug the SPI without Dependency Injection)
--------------------------------------------------------------------------------

export
evalReservation : ReservationExpr ty -> IO ty
evalReservation (Log msg) = putStrLn msg
evalReservation (Pure val) = pure val
evalReservation (Bind val next) = evalReservation val >>= evalReservation . next
evalReservation (SearchTrain dateTime) = pure ["T1", "T2"]
evalReservation (GetTypology trainId) = do
  putStrLn ("GetTypology: " ++ trainId)
  if trainId == "T1"
    then pure $ MkTrainTypology "T1" [MkCoachTypology "A" 100 [5..100]]
    else pure $ MkTrainTypology "T2" [MkCoachTypology "A" 100 [5..100]]

evalReservation (Reserve command) = do
  putStrLn ("Reserve: " ++ show command)
  pure $ Just command -- TODO: introduce errors
