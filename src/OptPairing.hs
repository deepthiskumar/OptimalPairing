
module OptPairing where

import qualified Data.Text as T



-- Optimal pairing that makes sense: For now, there are no conditions as to who
-- should be paired with whom, or how many time a tutor can be assigned.  All
-- we need to do is, for each subject, find a tutor for the student that requires one and pair
-- them up. Once the tutor list for each subject is exhausted, we start from the begining of the original list again.

-- Enhancement: Maybe add a predicate along with the tutor that decides if the tutor
-- is available for more allocations.
-- Tutor with max number of allocations allowed (Int -> Bool) or for that matter
-- any predicate that is required to be true for the tutor to be allocated. Tutot type would then be
-- data Tutor = Forall a => Tutor T.Text (a -> Bool)

type Subject = T.Text
type Student = T.Text
type Tutor = T.Text

--Input: modelling the input based on the specification in the question. This
--will also let us add tutors and students in constant time. However, updating
--tthis would be O(n)

type TutorElig = [(Tutor, [Subject])]
type StudentPref = [(Student, [Subject])]

--Ouput: Successfull pairing and students who haven't been assigned tutors
--because of unavailability, lets say. Our aim is to make sure all the
--students get a tutor and if not it should be notified

data OptPairs = OptPairs [(Tutor, Student)] [(Subject, Student)]
    deriving(Eq)

instance Show OptPairs where
    show p = showPairing p

emptyPairs = OptPairs [] []

--Intermediate types: List of students and tutors for each subject

type SubjectTutors = Subject -> [Tutor]
type SubjectStudents = Subject -> [Student]


subjectTutors :: TutorElig -> SubjectTutors
subjectTutors ts = \s -> lookUpAll s (concatMap (\p -> zip' (fst p) (snd p)) ts)

subjectStudents :: StudentPref -> SubjectStudents
subjectStudents ss = \s -> lookUpAll s (concatMap (\p -> zip' (fst p) (snd p)) ss)


pairs :: SubjectStudents -> SubjectTutors -> Subject -> OptPairs
pairs fs ft = \s -> case fs s of
    [] -> emptyPairs
    ss -> case ft s of
        [] -> OptPairs [] (zip' s ss)
        ts -> genPairing s ts ts ss


--Keep two copies of the tutor list instead of appending the tutor after being
--assigned to a student. Space vs time tradeoff. Time wins. Here, no tutor is
--assigned execcively if there are other tutors for the subject.

genPairing :: Subject -> [Tutor] -> [Tutor] -> [Student] -> OptPairs 
genPairing _ _ _ []              = OptPairs [] []
genPairing sub ts [] ss          = genPairing sub ts ts ss
genPairing sub ts (t:ts') (s:ss) = append (t,s) (genPairing sub ts ts' ss)


optimalPairing :: [Subject] -> TutorElig -> StudentPref -> OptPairs
optimalPairing subs tp sr = 
    appendMap (pairs (subjectStudents sr) (subjectTutors tp)) subs



--Helper functions

append :: (Tutor, Student) -> OptPairs -> OptPairs
append (t,s) (OptPairs ps es) = OptPairs ((t,s):ps) es 

appendMap :: (Subject -> OptPairs) -> [Subject] -> OptPairs
appendMap _ []     = OptPairs [] []
appendMap f (s:ss) = case f s of
    OptPairs ps es -> case appendMap f ss of
        OptPairs ps' es' -> OptPairs (ps++ps') (es++es')

lookUpAll :: Subject -> [(T.Text,Subject)] -> [T.Text]
lookUpAll s []  = []
lookUpAll s ((t,s'):xs) 
    | s == s'   = t:lookUpAll s xs 
    | otherwise = lookUpAll s xs

zip' :: T.Text -> [T.Text] -> [(T.Text,T.Text)]
zip' t []       = []
zip' t (s:ss)   = (t,s) : zip' t ss

showPairing :: OptPairs -> String
showPairing (OptPairs ps es) = "Optimal pairs:\n" ++ (show ps) ++ "\n\n"
  ++ "Unassigned students (Subject, Student):\n" ++ (show es) 



-------------------------------------------------------------------------------
--Immplementation 2: 


