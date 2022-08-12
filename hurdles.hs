

compareWords :: String -> String -> Bool
compareWords "" ""          = True
compareWords (x:xs) ""      = False
compareWords "" (y:ys)      = False
compareWords (x:xs) (y:ys)  = x == y && compareWords xs ys


compareWordsTests =
  [ compareWords "" ""
  , not $ compareWords "fred" ""
  , not $ compareWords "" "wilma"
  , compareWords "fred" "fred"
  , not $ compareWords "fred" "barney"
  ]


-- Given two strings of equal length, return a list of bools of if the character at the location is correct
characterCorrect :: String -> String -> [Bool]
characterCorrect "" ""         = []
characterCorrect (x:xs) ""     = [False]
characterCorrect "" (y:ys)     = [False]
characterCorrect (x:xs) (y:ys) = (x == y) : (characterCorrect xs ys)


characterCorrectTests =
  [ characterCorrect "Barney" "Barney"
  , characterCorrect "joe" "joey"
  , characterCorrect "joan" "joey"
  , characterCorrect "joel" "joey"
  , characterCorrect "joyce" "joe"
  ]

allWordTests = and compareWordsTests


-- characterLocation to flag this is the right character, or this is the right character in the wrong position
characterLocation :: Char -> String -> String -> Int
characterLocation x "" "" = 0
characterLocation x (y:ys) "" == 




-- Helper function to keep pushing in the missed items
characterWrongCorrectMissplaced :: String -> String -> String -> [Int]
characterWrongCorrectMissplaced "" "" ""      = []
-- characterWrongCorrectMissplaced (x:xs) (y:ys) ""



data ValueAndLocation = Correct | Incorrect | CorrectValue


-- given two strings of equal length, return a list of ValueAndLocation of whether the characters are correct
verifyGuess :: String -> String -> String -> [ValueAndLocation]
verifyGuess "" "" carries = []
verifyGuess (x:xs) (y:ys) carries =
  if
    x == y
  then
    Correct : verifyGuess xs ys carries
  else
    (
      if
        elem x ys || elem x carries
      then
        CorrectValue
      else
        Incorrect
    ) : verifyGuess xs ys (y:carries)
