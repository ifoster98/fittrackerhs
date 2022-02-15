module Fittracker where

import System.Process (getProcessExitCode)
import Data.Time.Calendar ( fromGregorian )
import Data.Time ( UTCTime(UTCTime), secondsToDiffTime )



type RepCount = Integer
type Weight = Double
type Error = String

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




inc :: ExerciseType -> Weight -> Double
inc Deadlift w = w + 5
inc _ w = w + 2.5

dec :: ExerciseType -> Weight -> Double
dec Deadlift w = w - 5
dec _ w = w - 2.5

getWeight :: Exercise -> Double
getWeight exercise = weight (head (sets exercise))

takeLast :: Int -> [a] -> [a]
takeLast n aList = reverse (take n (reverse aList))

getOutcome :: Exercise -> Outcome
getOutcome exercise = outcome (head (sets exercise))

calculateNextWeight :: ExerciseType -> Weight -> (Outcome, Outcome) -> Double
calculateNextWeight et w (Failure, Failure) = dec et w
calculateNextWeight et w (_, Failure) = w
calculateNextWeight et w (_, _) = inc et w

getLastWeight :: Either [Error] [Exercise] -> Double
getLastWeight (Left ers) = 0.0
getLastWeight (Right ex) = getWeight (last ex)

getLastTwoOutcomes :: Either [Error] [Exercise] -> (Outcome, Outcome)
getLastTwoOutcomes (Left ers) = (Failure, Failure)
getLastTwoOutcomes (Right ex) = (lastButOne, last)
  where last = getOutcome (head (takeLast 1 ex))
        lastButOne = getOutcome (head (takeLast 2 ex))

getNextWeight :: ExerciseType -> (ExerciseType -> Either [Error] [Exercise]) -> Double
getNextWeight et gExc = calculateNextWeight et lastWeight lastOutcomes
  where results = gExc et
        lastWeight = getLastWeight results
        lastOutcomes = getLastTwoOutcomes results

generateRep :: Weight -> RepCount -> Outcome -> Reps
generateRep w r o = Reps {weight = w, repCount = r, outcome = o}

generateRepsForNextWorkout :: Weight -> [Reps]
generateRepsForNextWorkout w = replicate 5 (generateRep w 5 Failure)

generateExercisesForNextWorkout :: WorkoutSubType -> (ExerciseType -> Either [Error] [Exercise]) -> [ExerciseType] -> [Exercise]
generateExercisesForNextWorkout wst _ [] = []
generateExercisesForNextWorkout wst gExc (x:xs) = Exercise {exerciseType = x, exerciseTime = Nothing, sets = reps}:generateExercisesForNextWorkout wst gExc xs
  where nextWeight = getNextWeight x gExc
        reps = generateRepsForNextWorkout nextWeight

getExercisesForWorkout :: WorkoutSubType -> [ExerciseType]
getExercisesForWorkout WorkoutA = [Squat, BenchPress, BentOverRows]
getExercisesForWorkout WorkoutB = [Squat, OverheadPress, Deadlift]

generateNextWorkout :: WorkoutSubType -> (ExerciseType -> Either [Error] [Exercise]) -> Workout
generateNextWorkout wst gExc = Workout { workoutType = FiveByFive, workoutSubType = wst, workoutTime = Nothing, exercises = exercises }
  where exercisesForWorkout = getExercisesForWorkout wst
        exercises = generateExercisesForNextWorkout wst gExc exercisesForWorkout




exerciseDay1 :: UTCTime
exerciseDay1 = UTCTime (fromGregorian 2022 02 01) (secondsToDiffTime 0)
exerciseDay2 :: UTCTime
exerciseDay2 = UTCTime (fromGregorian 2022 02 03) (secondsToDiffTime 0)
exerciseDay3 :: UTCTime
exerciseDay3 = UTCTime (fromGregorian 2022 02 05) (secondsToDiffTime 0)

getReps :: Double -> [Reps]
getReps w = replicate 5 r
  where
    r = Reps {weight = w, repCount = 5, outcome = Success}

s1 :: Exercise
s1 = Exercise {exerciseType = Squat, exerciseTime = Just exerciseDay1, sets = getReps 50}
s2 :: Exercise
s2 = Exercise {exerciseType = Squat, exerciseTime = Just exerciseDay2, sets = getReps 52.5}
s3 :: Exercise
s3 = Exercise {exerciseType = Squat, exerciseTime = Just exerciseDay3, sets = getReps 55}

d1 :: Exercise
d1 = Exercise {exerciseType = Deadlift, exerciseTime = Just exerciseDay1, sets = getReps 70}
d2 :: Exercise
d2 = Exercise {exerciseType = Deadlift, exerciseTime = Just exerciseDay2, sets = getReps 75}
d3 :: Exercise
d3 = Exercise {exerciseType = Deadlift, exerciseTime = Just exerciseDay3, sets = getReps 80}

b1 :: Exercise
b1 = Exercise {exerciseType = BenchPress, exerciseTime = Just exerciseDay1, sets = getReps 50}
b2 :: Exercise
b2 = Exercise {exerciseType = BenchPress, exerciseTime = Just exerciseDay2, sets = getReps 52.5}
b3 :: Exercise
b3 = Exercise {exerciseType = BenchPress, exerciseTime = Just exerciseDay3, sets = getReps 55}

o1 :: Exercise
o1 = Exercise {exerciseType = OverheadPress, exerciseTime = Just exerciseDay1, sets = getReps 30}
o2 :: Exercise
o2 = Exercise {exerciseType = OverheadPress, exerciseTime = Just exerciseDay2, sets = getReps 32.5}
o3 :: Exercise
o3 = Exercise {exerciseType = OverheadPress, exerciseTime = Just exerciseDay3, sets = getReps 35}

br1 :: Exercise
br1 = Exercise {exerciseType = BentOverRows, exerciseTime = Just exerciseDay1, sets = getReps 40}
br2 :: Exercise
br2 = Exercise {exerciseType = BentOverRows, exerciseTime = Just exerciseDay2, sets = getReps 42.5}
br3 :: Exercise
br3 = Exercise {exerciseType = BentOverRows, exerciseTime = Just exerciseDay3, sets = getReps 45}






saveExercise :: Exercise -> Either [Error] Outcome 
saveExercise e = Right Success

saveProposedWorkout :: Workout -> Either [Error] Outcome
saveProposedWorkout wkt = Right Success

getProposedWorkout :: WorkoutType -> WorkoutSubType -> Either [Error] Workout
getProposedWorkout wt wst =  Right wk
  where wk = Workout {workoutType = FiveByFive, workoutSubType = WorkoutA, workoutTime = Nothing, exercises = []}

getExercises :: ExerciseType -> Either [Error] [Exercise]
getExercises Squat = Right [s1, s2, s3]
getExercises Deadlift = Right [d1, d2, d3]
getExercises BenchPress = Right [b1, b2, b3]
getExercises OverheadPress = Right [o1, o2, o3]
getExercises BentOverRows = Right [br1, br2, br3]







getNextWorkout :: WorkoutType -> WorkoutSubType -> Either [Error] Workout
getNextWorkout = getProposedWorkout

saveWorkout :: Workout -> Either [Error] Outcome
saveWorkout w = Right Success
-- Write incoming message to log
-- Save each exercise in the incoming workout separately to the repository
-- Generate the next workout in this sequence and save to repository
