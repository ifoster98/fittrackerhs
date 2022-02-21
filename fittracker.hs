import Domain ( ExerciseType(Squat) )
import InMemDatabase ( newWorkout, database )
import Repository ( getExercises, saveExercises )

main :: IO ()
main = do
  let updatedDatabase = saveExercises database newWorkout
  let squats = getExercises updatedDatabase Squat
  print squats
