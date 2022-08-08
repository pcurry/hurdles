

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
