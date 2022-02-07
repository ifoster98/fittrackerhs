module InMemRepository where

import Domain
import Data.Time.Calendar ( fromGregorian )
import Data.Time ( UTCTime(UTCTime), secondsToDiffTime )

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