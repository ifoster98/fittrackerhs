import System.Process (getProcessExitCode)
import Data.Time.Calendar ( fromGregorian )
import Data.Time ( UTCTime(UTCTime), secondsToDiffTime )
import Data.Map (Map)
import qualified Data.Map as Map
import Domain

inc :: ExerciseType -> Weight -> Weight
inc Deadlift w = w + 5
inc _ w = w + 2.5

dec :: ExerciseType -> Weight -> Weight
dec Deadlift w = w - 5
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

nextExercises = [s4, b4, d4]


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

dbExercises :: Map ExerciseType [Exercise]
dbExercises = Map.fromList keyVals
  where keys = [Squat, Deadlift, BenchPress, OverheadPress, BentOverRows]
        vals = [squats, deadlifts, benchPresses, overheadPresses, bentOverRows]
        keyVals = zip keys vals

database = Database {exerciseLookup = dbExercises, proposedWorkout = Nothing}





saveExercise :: Database -> Exercise -> Database
saveExercise db e = Database {exerciseLookup = newElu, proposedWorkout = proposedWorkout db}
  where elu = exerciseLookup db
        et = exerciseType e
        es = getExercises db et
        f x  = Just (e:es)
        newElu = Map.update f et elu

saveProposedWorkout :: Database -> Workout -> Database
saveProposedWorkout db wkt = Database {exerciseLookup = exerciseLookup db, proposedWorkout = Just wkt}

listOrEmpty :: Maybe [Exercise] -> [Exercise]
listOrEmpty Nothing = []
listOrEmpty (Just es) = es

getExercises :: Database -> ExerciseType -> [Exercise]
getExercises db et = listOrEmpty (Map.lookup et (exerciseLookup db))




-- Write incoming message to log
-- Generate the next workout in this sequence and save to repository

saveExercises :: Database -> [Exercise] -> Database
saveExercises db exs = foldl (<>) db dbs
  where f = saveExercise db
        dbs = map f exs

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






main :: IO ()
main = do
  let d = saveExercises database nextExercises
  let squats = getExercises d Squat
  print squats
