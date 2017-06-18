module ReservationEval

import ReservationExpr


--------------------------------------------------------------------------------
-- Interpreter: Transformation from the abstract problem to real world
--
-- Here, we implement a fake interpreter than for the example
--------------------------------------------------------------------------------

export
interface SPI spi where
  searchTrainAt : spi -> DateTime -> IO (List TrainId)
  getTypologyOf : spi -> TrainId -> IO (Maybe TrainTypology)
  confirmCommand : spi -> Reservation -> IO (Maybe Reservation)

export
evalReservation : (SPI spi) => spi -> ReservationExpr ty -> IO ty
evalReservation spi (Log msg) = putStrLn msg
evalReservation spi (Pure val) = pure val
evalReservation spi (Bind val next) = evalReservation spi val >>= evalReservation spi . next
evalReservation spi (SearchTrain time) = searchTrainAt spi time
evalReservation spi (GetTypology trainId) = getTypologyOf spi trainId
evalReservation spi (Reserve command) = confirmCommand spi command
