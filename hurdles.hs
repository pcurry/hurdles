foo = True
bar = False

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


allTests = and compareWordsTests
