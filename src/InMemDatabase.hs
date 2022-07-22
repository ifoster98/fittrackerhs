module InMemDatabase where

import Data.Time.Calendar ( fromGregorian )
import Data.Time ( UTCTime(UTCTime), secondsToDiffTime )
import qualified Data.Map as Map
import Domain
    ( Database(Database, exerciseLookup, proposedWorkout),
      Exercise(..),
      ExerciseType(BentOverRows, Squat, Deadlift, BenchPress,
                   OverheadPress),
      Outcome(Success),
      Reps(..),
      Workout(..),
      WorkoutSubType(WorkoutA),
      WorkoutType(FiveByFive),
      ExerciseLookup )

exerciseDay1 :: UTCTime
exerciseDay1 = UTCTime (fromGregorian 2022 02 01) (secondsToDiffTime 0)
exerciseDay2 :: UTCTime
exerciseDay2 = UTCTime (fromGregorian 2022 02 03) (secondsToDiffTime 0)
exerciseDay3 :: UTCTime
exerciseDay3 = UTCTime (fromGregorian 2022 02 05) (secondsToDiffTime 0)
exerciseDay4 :: UTCTime
exerciseDay4 = UTCTime (fromGregorian 2022 02 07) (secondsToDiffTime 0)

getReps :: Double -> [Reps]
getReps w = replicate 5 r
  where
    r = Reps {weight = w, repCount = 5, outcome = Success}

s1 :: Exercise
s1 = Exercise {exerciseType = Squat, exerciseTime = Just exerciseDay3, sets = getReps 55}
s2 :: Exercise
s2 = Exercise {exerciseType = Squat, exerciseTime = Just exerciseDay2, sets = getReps 52.5}
s3 :: Exercise
s3 = Exercise {exerciseType = Squat, exerciseTime = Just exerciseDay1, sets = getReps 50}

d1 :: Exercise
d1 = Exercise {exerciseType = Deadlift, exerciseTime = Just exerciseDay3, sets = getReps 80}
d2 :: Exercise
d2 = Exercise {exerciseType = Deadlift, exerciseTime = Just exerciseDay2, sets = getReps 75}
d3 :: Exercise
d3 = Exercise {exerciseType = Deadlift, exerciseTime = Just exerciseDay1, sets = getReps 70}

b1 :: Exercise
b1 = Exercise {exerciseType = BenchPress, exerciseTime = Just exerciseDay3, sets = getReps 55}
b2 :: Exercise
b2 = Exercise {exerciseType = BenchPress, exerciseTime = Just exerciseDay2, sets = getReps 52.5}
b3 :: Exercise
b3 = Exercise {exerciseType = BenchPress, exerciseTime = Just exerciseDay1, sets = getReps 50}

o1 :: Exercise
o1 = Exercise {exerciseType = OverheadPress, exerciseTime = Just exerciseDay3, sets = getReps 35}
o2 :: Exercise
o2 = Exercise {exerciseType = OverheadPress, exerciseTime = Just exerciseDay2, sets = getReps 32.5}
o3 :: Exercise
o3 = Exercise {exerciseType = OverheadPress, exerciseTime = Just exerciseDay1, sets = getReps 30}

br1 :: Exercise
br1 = Exercise {exerciseType = BentOverRows, exerciseTime = Just exerciseDay3, sets = getReps 45}
br2 :: Exercise
br2 = Exercise {exerciseType = BentOverRows, exerciseTime = Just exerciseDay2, sets = getReps 42.5}
br3 :: Exercise
br3 = Exercise {exerciseType = BentOverRows, exerciseTime = Just exerciseDay1, sets = getReps 40}

s4 :: Exercise
s4 = Exercise {exerciseType = Squat, exerciseTime = Just exerciseDay4, sets = getReps 57.5}
d4 :: Exercise
d4 = Exercise {exerciseType = Deadlift, exerciseTime = Just exerciseDay4, sets = getReps 85}
b4 :: Exercise
b4 = Exercise {exerciseType = BenchPress, exerciseTime = Just exerciseDay4, sets = getReps 57.5}

newWorkout :: Workout
newWorkout = Workout{
  workoutType = FiveByFive, 
  workoutSubType = WorkoutA, 
  workoutTime = Just (UTCTime (fromGregorian 2022 02 09) (secondsToDiffTime 0)), 
  exercises = [s4,b4,d4]}


squats :: [Exercise]
squats = [s1, s2, s3]
deadlifts :: [Exercise]
deadlifts = [d1, d2, d3]
benchPresses :: [Exercise]
benchPresses = [b1, b2, b3]
overheadPresses :: [Exercise]
overheadPresses = [o1, o2, o3]
bentOverRows :: [Exercise]
bentOverRows = [br1, br2, br3]

database :: Database
database = Database {exerciseLookup = dbExercises, proposedWorkout = Nothing}

dbExercises :: ExerciseLookup
dbExercises = Map.fromList keyVals
  where keys = [Squat, Deadlift, BenchPress, OverheadPress, BentOverRows]
        vals = [squats, deadlifts, benchPresses, overheadPresses, bentOverRows]
        keyVals = zip keys vals
