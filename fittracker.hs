module Fittracker where

import FiveByFive ( generateNextWorkout )
import Domain
import InMemRepository ( saveExercise, getProposedWorkout )
import System.Process (getProcessExitCode)

getNextWorkout :: WorkoutType -> WorkoutSubType -> Either [Error] Workout
getNextWorkout = getProposedWorkout

saveWorkout :: Workout -> Either [Error] Outcome
saveWorkout w = Right Success
-- Write incoming message to log
-- Save each exercise in the incoming workout separately to the repository
-- Generate the next workout in this sequence and save to repository
