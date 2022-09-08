

compareWords :: String -> String -> Bool
compareWords "" ""          = True
compareWords (_:_) ""      = False
compareWords "" (_:_)      = False
compareWords (x:xs) (y:ys)  = x == y && compareWords xs ys


compareWordsTests :: [Bool]
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
characterCorrect (_:_) ""      = [False]
characterCorrect "" (_:_)      = [False]
characterCorrect (x:xs) (y:ys) = (x == y) : (characterCorrect xs ys)


characterCorrectTests :: [[Bool]]
characterCorrectTests =
  [ characterCorrect "Barney" "Barney"
  , characterCorrect "joe" "joey"
  , characterCorrect "joan" "joey"
  , characterCorrect "joel" "joey"
  , characterCorrect "joyce" "joe"
  ]

allWordTests :: Bool
allWordTests = and compareWordsTests


data ValueAndLocation = Correct | Incorrect | CorrectValue deriving Show

instance Eq ValueAndLocation where
  Correct      == Correct       = True
  Incorrect    == Incorrect     = True
  CorrectValue == CorrectValue  = True
  _            == _             = False



removeFirst :: Eq a => a -> [a] -> [a] 
removeFirst _ [] = []
removeFirst x (y:ys) | x == y    = ys
                     | otherwise = y : removeFirst x ys


-- given two strings of equal length, return a list of ValueAndLocation of whether the characters are correct
verifyGuess :: String -> String -> [ValueAndLocation]
verifyGuess xs ys = verifyGuess' xs ys ys


-- given two strings of equal length, return a list of ValueAndLocation of whether the characters are correct
verifyGuess' :: String -> String -> String -> [ValueAndLocation]
verifyGuess' "" "" _ = []
verifyGuess' (x:xs) (y:ys) carries | x == y = Correct : verifyGuess' xs ys carries'
                                   | elem x carries = CorrectValue : verifyGuess' xs ys carries'
                                   | otherwise = Incorrect : verifyGuess' xs ys carries
  where carries' = removeFirst x carries

verifyGuess' _ _ _ = error "Inconceivable!"
