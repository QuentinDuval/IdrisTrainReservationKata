module ReservationExpr

%access public export


--------------------------------------------------------------------------------
-- Domain types (concepts)
--------------------------------------------------------------------------------

TrainId : Type
TrainId = String

CoachId : Type
CoachId = String

SeatId : Type
SeatId = Int

DateTime : Type
DateTime = Int

record ReservationRequest where
  constructor MkReservationRequest
  seatCount : Nat
  dateTime : DateTime

record CoachTypology where
  constructor MkCoachTypology
  coachId : CoachId
  totalSeatCount : Nat
  availableSeats : List SeatId

record TrainTypology where
  constructor MkTrainTypology
  trainId : TrainId
  coaches : List CoachTypology

record Reservation where -- TODO: use phantom type for the confirmation
  constructor MkReservation
  trainId : TrainId
  coachId : CoachId
  seatNbs : List SeatId

Show Reservation where
  show r = "{" ++ show (trainId r) ++ ", "
               ++ show (coachId r )++ ","
               ++ show (seatNbs r) ++ "}"

Eq Reservation where
  r1 == r2 = toTuple r1 == toTuple r2
    where
      toTuple : Reservation -> (TrainId, CoachId, List SeatId)
      toTuple r = (trainId r, coachId r, seatNbs r)

data ReservationResult
  = Confirmed Reservation
  | NoTrainAvailable

Show ReservationResult where
  show (Confirmed r) = "Confirmed: " ++ show r
  show NoTrainAvailable = "No Trains Available"


--------------------------------------------------------------------------------
-- Definition of a DSL for reservation
--------------------------------------------------------------------------------

data ReservationExpr : Type -> Type where
  -- TODO: add state... to force a workflow (and add abort + confirm + pay)
  SearchTrain : DateTime -> ReservationExpr (List TrainId)
  GetTypology : TrainId -> ReservationExpr TrainTypology
  Reserve : Reservation -> ReservationExpr (Maybe Reservation)
  Log : String -> ReservationExpr ()
  Pure : ta -> ReservationExpr ta
  Bind : ReservationExpr ta -> (ta -> ReservationExpr tb) -> ReservationExpr tb

(>>=): ReservationExpr ta -> (ta -> ReservationExpr tb) -> ReservationExpr tb
(>>=) = Bind

Functor ReservationExpr where
  map fn expr = expr >>= Pure . fn

Applicative ReservationExpr where
  pure = Pure
  fExpr <*> aExpr = fExpr >>= \f => map f aExpr
