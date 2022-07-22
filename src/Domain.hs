module Domain where

import Data.Time ( UTCTime(UTCTime))
import Data.Map (Map)
import qualified Data.Map as Map

type RepCount = Integer
type Weight = Double
type ExerciseLookup = Map ExerciseType [Exercise]

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
  exerciseLookup :: ExerciseLookup
  , proposedWorkout :: Maybe Workout
} deriving (Show, Eq, Ord)

mergeExercises :: Ord k => Map k [a] -> Map k [a] -> Map k [a]
mergeExercises = Map.unionWith longestList
  where longestList l1 l2 = if length l1 > length l2 then l1 else l2

mergeProposedWorkouts :: Maybe a -> Maybe a -> Maybe a
mergeProposedWorkouts Nothing Nothing = Nothing
mergeProposedWorkouts (Just p1) Nothing = Just p1
mergeProposedWorkouts Nothing (Just p1) = Just p1
mergeProposedWorkouts p1 p2 = p2

mergeDatabases :: Database -> Database -> Database
mergeDatabases d1 d2 = Database {proposedWorkout = mpw, exerciseLookup = elu}
  where mpw = mergeProposedWorkouts (proposedWorkout d1) (proposedWorkout d2)
        elu = mergeExercises (exerciseLookup d1) (exerciseLookup d2)

instance Semigroup Database where
  (<>) d1 d2 = Database {proposedWorkout = mpw, exerciseLookup = elu}
    where mpw = mergeProposedWorkouts (proposedWorkout d1) (proposedWorkout d2)
          elu = mergeExercises (exerciseLookup d1) (exerciseLookup d2)