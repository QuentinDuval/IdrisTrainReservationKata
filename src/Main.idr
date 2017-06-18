module Main

import ReservationApi
import ReservationExpr
import ReservationEval


--------------------------------------------------------------------------------
-- Test version with a Fake SPI implementation
--------------------------------------------------------------------------------

data FakeSPI = MkFakeSPI (List TrainTypology)

SPI FakeSPI where
  searchTrainAt (MkFakeSPI db) _ = pure (map trainId db)
  getTypologyOf (MkFakeSPI db) tid = pure $ find ((== tid). trainId) db
  confirmCommand (MkFakeSPI db) r = pure (Just r)

fakeTrainDB : FakeSPI
fakeTrainDB = MkFakeSPI
  [ MkTrainTypology "T1" [MkCoachTypology "A" 100 [71..100], MkCoachTypology "B" 100 [71..100]] -- Full Train
  , MkTrainTypology "T2" [MkCoachTypology "A" 100 [80..100], MkCoachTypology "B" 100 [50..100]] -- First coach full
  , MkTrainTypology "T3" [MkCoachTypology "A" 100 [10..100], MkCoachTypology "B" 100 [1..100]] -- Plenty of places
  ]

sendReservation : Nat -> IO ()
sendReservation seatCount = do
  result <- evalReservation fakeTrainDB $ reserve (MkReservationRequest seatCount 10)
  printLn result

main : IO ()
main = do
  putStrLn "> [Test] Number of seats to reserve:"
  seatCount <- map cast getLine
  when (seatCount > 0) $ do
    sendReservation seatCount
    main
