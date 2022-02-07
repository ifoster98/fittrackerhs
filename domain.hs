module Domain where

import Data.Time

type RepCount = Integer
type Weight = Double

data Outcome = Success | Failure deriving (Show, Eq)
data WorkoutType = FiveByFive | MadCow | UpperLowerSplit deriving (Show, Eq)
data WorkoutSubType = WorkoutA | WorkoutB deriving (Show, Eq)
data ExerciseType = Squat | BenchPress | Deadlift | OverheadPress | BentOverRows deriving (Show, Eq)
data Reps = Reps {
  weight :: Weight
  , repCount ::  RepCount
  , outcome :: Outcome }

data Exercise = Exercise {
  exerciseType :: ExerciseType
  , exerciseTime :: Maybe UTCTime
  , sets :: [Reps] }

data Workout = Workout {
  workoutType :: WorkoutType
  , workoutSubType :: WorkoutSubType
  , workoutTime :: Maybe UTCTime
  , exercises :: [Exercise] }

showReps :: Reps -> String
showReps rps = show (weight rps) ++ " "  ++ show (repCount rps) ++ " " ++ show (outcome rps)

showSet :: [Reps] -> String
showSet [] = ""
showSet (x:xs) = showReps x ++ " " ++ showSet xs

showExercise :: Exercise -> String
showExercise ex = show (exerciseType ex) ++ " " ++ showSet (sets ex)

showExercises :: [Exercise] -> String
showExercises [] = ""
showExercises (x:xs) = showExercise x ++ " " ++ showExercises xs

showWorkout :: Workout -> String
showWorkout wk = show (workoutType wk) ++ " " ++ show (workoutSubType wk) ++ " " ++ showExercises (exercises wk)
