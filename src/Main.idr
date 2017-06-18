module Main

import ReservationApi
import ReservationExpr
import ReservationEval


--------------------------------------------------------------------------------
-- OVERALL DESIGN, separation between:
-- * A DSL: The rules for a valid implementation of reservation, the vocabulary
--   from the outside, and abstract specification of its contact with SPIs
-- * The EVAL: The translation of the DSL into the real world. In particular,
--   it handles the business with the SPIs (and their errors)
-- * The DOMAIN CODE: The implementation of the reservation in the context of
--   the rules stated inside the Reservation DSL
--
-- The decoupling is pretty good and stronger than with Hexagonal architecture:
-- * Error handling does not have to traverse the domain code
-- * We can enforce rules and workflow in the DSL by the type system
-- * The evaluator can handle very technical stuff without the code knowing
-- * By having a dedicated DSL Monad, you can ensure the code follows the rules
--------------------------------------------------------------------------------

sendReservation : Nat -> IO ()
sendReservation seatCount = do
  result <- evalReservation $ reserve (MkReservationRequest seatCount 10)
  printLn result

main : IO ()
main = do
  putStrLn "> [Test] Number of seats to reserve:"
  seatCount <- map cast getLine
  when (seatCount > 0) $ do
    sendReservation seatCount
    main
