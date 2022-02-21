module Domain where

import Data.Time ( UTCTime(UTCTime))
import Data.Map (Map)
import qualified Data.Map as Map

type RepCount = Integer
type Weight = Double

data Outcome = Success | Failure deriving (Show)
instance Eq Outcome where
    (==) Success Success = True
    (==) Failure Failure = True
    (==) _ _ = False
instance Ord Outcome where
    compare Success Success = EQ
    compare Failure Failure = EQ
    compare Success Failure = GT
    compare Failure Success = LT

data WorkoutType = FiveByFive | MadCow | UpperLowerSplit deriving (Show, Eq, Ord)
data WorkoutSubType = WorkoutA | WorkoutB deriving (Show, Eq, Ord)
data ExerciseType = Squat | BenchPress | Deadlift | OverheadPress | BentOverRows deriving (Show, Eq, Ord)
data Reps = Reps {
  weight :: Weight
  , repCount ::  RepCount
  , outcome :: Outcome } deriving (Show, Eq, Ord)

data Exercise = Exercise {
  exerciseType :: ExerciseType
  , exerciseTime :: Maybe UTCTime
  , sets :: [Reps] } deriving (Show, Eq, Ord)

data Workout = Workout {
  workoutType :: WorkoutType
  , workoutSubType :: WorkoutSubType
  , workoutTime :: Maybe UTCTime
  , exercises :: [Exercise] } deriving (Show, Eq, Ord)

data Database = Database {
  exerciseLookup :: Map ExerciseType [Exercise]
  , proposedWorkout :: Maybe Workout
} deriving (Show, Eq, Ord)
