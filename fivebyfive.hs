{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module FiveByFive where

import Domain
import InMemRepository ( getExercises )

getExercisesForWorkout :: WorkoutType -> WorkoutSubType -> [ExerciseType]
getExercisesForWorkout FiveByFive WorkoutA = [Squat, BenchPress, BentOverRows]
getExercisesForWorkout FiveByFive WorkoutB = [Squat, OverheadPress, Deadlift]

inc :: ExerciseType -> Weight -> Double
inc Deadlift w = w + 5
inc _ w = w + 2.5

dec :: ExerciseType -> Weight -> Double
dec Deadlift w = w - 5
dec _ w = w - 2.5

calculateNextWeight :: ExerciseType -> Weight -> (Outcome, Outcome) -> Double
calculateNextWeight et w (Failure, Failure) = dec et w
calculateNextWeight et w (_, Failure) = w
calculateNextWeight et w (_, _) = inc et w

takeLast :: Int -> [a] -> [a]
takeLast n aList = reverse (take n (reverse aList))

getWeight :: Exercise -> Double
getWeight exercise = weight (head (sets exercise))

getOutcome :: Exercise -> Outcome
getOutcome exercise = outcome (head (sets exercise))

getLastWeight :: Either [Error] [Exercise] -> Double
getLastWeight (Left ers) = 0.0
getLastWeight (Right ex) = getWeight (last ex)

getLastTwoOutcomes :: Either [Error] [Exercise] -> (Outcome, Outcome)
getLastTwoOutcomes (Left ers) = (Failure, Failure)
getLastTwoOutcomes (Right ex) = (lastButOne, last)
  where last = getOutcome (head (takeLast 1 ex))
        lastButOne = getOutcome (head (takeLast 2 ex))

generateRep :: Weight -> RepCount -> Outcome -> Reps
generateRep w r o = Reps {weight = w, repCount = r, outcome = o}

generateRepsForNextWorkout :: WorkoutType -> Weight -> [Reps]
generateRepsForNextWorkout FiveByFive w = replicate 5 (generateRep w 5 Failure)

getNextWeight :: ExerciseType -> Double
getNextWeight et = calculateNextWeight et lastWeight lastOutcomes
  where results = getExercises et
        lastWeight = getLastWeight results
        lastOutcomes = getLastTwoOutcomes results

generateExercisesForNextWorkout :: WorkoutType -> WorkoutSubType -> [ExerciseType] -> [Exercise]
generateExercisesForNextWorkout FiveByFive wst [] = []
generateExercisesForNextWorkout FiveByFive wst (x:xs) = Exercise {exerciseType = x, exerciseTime = Nothing, sets = reps}:generateExercisesForNextWorkout FiveByFive wst xs
  where nextWeight = getNextWeight x
        reps = generateRepsForNextWorkout FiveByFive nextWeight

generateNextWorkout :: WorkoutType -> WorkoutSubType -> Workout
generateNextWorkout wt wst = Workout { workoutType = wt, workoutSubType = wst, workoutTime = Nothing, exercises = exercises }
  where exercisesForWorkout = getExercisesForWorkout wt wst
        exercises = generateExercisesForNextWorkout wt wst exercisesForWorkout
