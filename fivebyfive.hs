module FiveByFive(generateNextWorkout) where

import Domain

getExercisesForWorkout :: WorkoutSubType -> [ExerciseType]
getExercisesForWorkout WorkoutA = [Squat, BenchPress, BentOverRows]
getExercisesForWorkout WorkoutB = [Squat, OverheadPress, Deadlift]

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

generateRepsForNextWorkout :: Weight -> [Reps]
generateRepsForNextWorkout w = replicate 5 (generateRep w 5 Failure)

getNextWeight :: ExerciseType -> (ExerciseType -> Either [Error] [Exercise]) -> Double
getNextWeight et gExc = calculateNextWeight et lastWeight lastOutcomes
  where results = gExc et
        lastWeight = getLastWeight results
        lastOutcomes = getLastTwoOutcomes results

generateExercisesForNextWorkout :: WorkoutSubType -> (ExerciseType -> Either [Error] [Exercise]) -> [ExerciseType] -> [Exercise]
generateExercisesForNextWorkout wst _ [] = []
generateExercisesForNextWorkout wst gExc (x:xs) = Exercise {exerciseType = x, exerciseTime = Nothing, sets = reps}:generateExercisesForNextWorkout wst gExc xs
  where nextWeight = getNextWeight x gExc
        reps = generateRepsForNextWorkout nextWeight

generateNextWorkout :: WorkoutSubType -> (ExerciseType -> Either [Error] [Exercise]) -> Workout
generateNextWorkout wst gExc = Workout { workoutType = FiveByFive, workoutSubType = wst, workoutTime = Nothing, exercises = exercises }
  where exercisesForWorkout = getExercisesForWorkout wst
        exercises = generateExercisesForNextWorkout wst gExc exercisesForWorkout
