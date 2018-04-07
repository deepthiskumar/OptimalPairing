module Main where

import OptPairing
import qualified Data.Text as T

main :: IO ()
main = do
    let res = optimalPairing subjects tutorElig studPref
    print res
    print (test == res)

mack = map T.pack

subjects :: [Subject]
subjects = mack ["ACT Reading", "ACT Writing", "ACT Essay", "ACT Science", "ACT Math", "US History", "AP Chemistry"]

tutorElig :: TutorElig
tutorElig = [ (T.pack "Joakim", mack ["ACT Reading", "ACT Essay", "ACT Writing"]),
   (T.pack "Bob", mack ["AP Chemistry", "ACT Science"] ),
   (T.pack "Frieda", mack ["ACT Essay", "ACT Writing"] ),
   (T.pack "Kseniya", mack ["US History"]),
   (T.pack "Maggie", mack ["ACT Science"]),
   (T.pack "Jamal", mack ["ACT Math"])
   ]

studPref :: StudentPref
studPref = [ (T.pack "Annie", mack ["ACT Math"]),
   (T.pack "Oskar", mack ["AP Chemistry"]),
   (T.pack "Olle", mack ["ACT Reading", "ACT Writing", "ACT Essay", "ACT Science", "ACT Math"]),
   (T.pack "Ingrid", mack ["US History", "AP Chemistry"]),
   (T.pack "Yuchen", mack ["ACT Math"]),
   (T.pack "Arjun", mack ["ACT Reading", "ACT Writing", "ACT Essay"]),
   (T.pack "Esmeralda", mack ["AP Chemistry"])
   ]

test = OptPairs (map (\p -> (T.pack (fst p), T.pack (snd p))) [("Joakim","Olle"),("Joakim","Arjun"),("Joakim","Olle"),
    ("Frieda","Arjun"),("Joakim","Olle"),("Frieda","Arjun"),("Bob","Olle"),
    ("Jamal","Annie"),("Jamal","Olle"),("Jamal","Yuchen"),("Kseniya","Ingrid"),
    ("Bob","Oskar"),("Bob","Ingrid"),("Bob","Esmeralda")]) []
