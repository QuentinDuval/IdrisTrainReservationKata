module Main

import ReservationApi
import ReservationExpr
import ReservationEval

main : IO ()
main = do
  putStrLn "Acceptance Tests"
  let request = MkReservationRequest 10 10
  result <- evalReservation (reserve request)
  printLn result
