module FiveByFive where

import Domain
    ( Exercise(..),
      ExerciseType(..),
      Outcome(Failure),
      RepCount,
      Reps(..),
      Weight,
      Workout(..),
      WorkoutSubType(..),
      WorkoutType(FiveByFive) )

inc :: ExerciseType -> Weight -> Weight
inc Deadlift w = w + 5
inc _ w = w + 2.5

dec :: ExerciseType -> Weight -> Weight
dec Deadlift 5 = 5
dec Deadlift w = w - 5
dec _ 2.5 = 2.5
dec _ w = w - 2.5

takeLast :: Int -> [a] -> [a]
takeLast n aList = reverse (take n (reverse aList))

getWeight :: Exercise -> Weight
getWeight exercise = weight (head (sets exercise))

getOutcome :: Exercise -> Outcome
getOutcome exercise = outcome (head (sets exercise))

calculateNextWeight :: ExerciseType -> Weight -> (Outcome, Outcome) -> Weight
calculateNextWeight et w (Failure, Failure) = dec et w
calculateNextWeight et w (_, Failure) = w
calculateNextWeight et w (_, _) = inc et w

getLastWeight :: [Exercise] -> Weight
getLastWeight ex = getWeight (last ex)

getLastTwoOutcomes :: [Exercise] -> (Outcome, Outcome)
getLastTwoOutcomes ex = (lastButOne, last)
  where last = getOutcome (head (takeLast 1 ex))
        lastButOne = getOutcome (head (takeLast 2 ex))

getNextWeight :: ExerciseType -> (ExerciseType -> [Exercise]) -> Weight
getNextWeight et gExc = calculateNextWeight et lastWeight lastOutcomes
  where results = gExc et
        lastWeight = getLastWeight results
        lastOutcomes = getLastTwoOutcomes results

generateRep :: Weight -> RepCount -> Outcome -> Reps
generateRep w r o = Reps {weight = w, repCount = r, outcome = o}

generateRepsForNextWorkout :: Weight -> [Reps]
generateRepsForNextWorkout w = replicate 5 (generateRep w 5 Failure)

generateExercisesForNextWorkout :: WorkoutSubType -> (ExerciseType -> [Exercise]) -> [ExerciseType] -> [Exercise]
generateExercisesForNextWorkout wst _ [] = []
generateExercisesForNextWorkout wst gExc (x:xs) = Exercise {exerciseType = x, exerciseTime = Nothing, sets = reps}:generateExercisesForNextWorkout wst gExc xs
  where nextWeight = getNextWeight x gExc
        reps = generateRepsForNextWorkout nextWeight

getExercisesForWorkout :: WorkoutSubType -> [ExerciseType]
getExercisesForWorkout WorkoutA = [Squat, BenchPress, BentOverRows]
getExercisesForWorkout WorkoutB = [Squat, OverheadPress, Deadlift]

getNextWorkoutSubType :: WorkoutSubType -> WorkoutSubType
getNextWorkoutSubType WorkoutA = WorkoutB
getNextWorkoutSubType _ = WorkoutA

generateNextWorkout :: Workout -> (ExerciseType -> [Exercise]) -> Workout
generateNextWorkout w gExc = Workout { workoutType = FiveByFive, workoutSubType = wst, workoutTime = Nothing, exercises = exercises }
  where wst = getNextWorkoutSubType (workoutSubType w)
        exercisesForWorkout = getExercisesForWorkout wst
        exercises = generateExercisesForNextWorkout wst gExc exercisesForWorkout
