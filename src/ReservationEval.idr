module ReservationEval

import ReservationExpr


--------------------------------------------------------------------------------
-- Interpreter: Transformation from the abstract problem to real world (fake one here)
--------------------------------------------------------------------------------

export
interface SPI spi where
  searchTrainAt : spi -> DateTime -> IO (List TrainId)
  getTypologyOf : spi -> TrainId -> IO (Maybe TrainTypology)
  confirmCommand : spi -> Reservation -> IO (Maybe Reservation)

export
evalReservation : SPI spi => spi -> ReservationExpr ty -> IO ty
evalReservation spi = evalCmd
  where
    evalCmd : ReservationExpr ty -> IO ty
    evalCmd (Log msg) = putStrLn msg
    evalCmd (Pure val) = pure val
    evalCmd (Bind val next) = evalCmd val >>= evalCmd . next
    evalCmd (SearchTrain time) = searchTrainAt spi time
    evalCmd (GetTypology trainId) = getTypologyOf spi trainId
    evalCmd (Reserve command) = confirmCommand spi command
