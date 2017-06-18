# Train Reservation Kata (Idris)

An implementation of a problem close to the challenged proposed in:
https://github.com/emilybache/KataTrainReservation

## Goal

The goal is to implement the domain of train reservation:
* We should offer an API to reserve a given number of seats at a given date
* The domain relies on Service Provider Interfaces (SPI)
* The implementation should make sure to be as decoupled as possible to the SPI

## Rules

The rules of the reservation of a train are the following:
* We cannot reserve seats in a train if it bumps up the occupancy over 70%
* All the reserved seats should be in the same coach (we cannot separate families)
* Preferably, we should avoid bumping the occupancy of a coach over 70%
