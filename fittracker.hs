module Fittracker where

import FiveByFive
import Domain
import InMemRepository

getNextWorkout :: WorkoutType -> WorkoutSubType -> Workout
getNextWorkout wt wst = Workout {workoutType = wt, workoutSubType = wst, workoutTime = Nothing, exercises = []}

saveWorkout :: Workout -> Either [Error] Outcome
saveWorkout w = Right Success
