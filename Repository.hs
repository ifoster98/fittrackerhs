module Repository where

import Domain
    ( Database(..),
      Exercise(exerciseType),
      ExerciseType,
      Workout(exercises) )
import qualified Data.Map as Map

saveExercise :: Database -> Exercise -> Database
saveExercise db e = Database {exerciseLookup = newElu, proposedWorkout = proposedWorkout db}
  where elu = exerciseLookup db
        et = exerciseType e
        es = getExercises db et
        f x  = Just (e:es)
        newElu = Map.update f et elu

saveProposedWorkout :: Database -> Workout -> Database
saveProposedWorkout db wkt = Database {exerciseLookup = exerciseLookup db, proposedWorkout = Just wkt}

getExercises :: Database -> ExerciseType -> [Exercise]
getExercises db et = listOrEmpty (Map.lookup et (exerciseLookup db))
  where listOrEmpty Nothing = []
        listOrEmpty (Just es) = es

saveExercises :: Database -> Workout -> Database
saveExercises db wkt = foldl (<>) db dbs
  where f = saveExercise db
        exs = exercises wkt
        dbs = map f exs
