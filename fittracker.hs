module Fittracker where

import FiveByFive
import Domain
import InMemRepository

getNextWorkout :: WorkoutType -> WorkoutSubType -> Workout
getNextWorkout wt wst = Workout {workoutType = wt, workoutSubType = wst, workoutTime = Nothing, exercises = []}
-- Call repository to get next workout in sequence

saveWorkout :: Workout -> Either [Error] Outcome
saveWorkout w = Right Success
-- Write incoming messag to log
-- Save each exercise in the incoming workout separately to the repository
-- Generate the next workout in this sequence and save to repository
