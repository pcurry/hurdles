
thingy :: String -> String
thingy xs = zipWith const (drop 1 xs) (drop 2 xs)

drop' :: Int -> [a] -> [a]
drop' _ []     = []
drop' 0 xs     = xs 
drop' n (_:xs) = drop' (pred n) xs

const' :: a -> b -> a
const' x _ = x

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _          = []
zip' _ []          = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

with' :: (a -> b -> c) -> [(a,b)] -> [c]
with' f = fmap (uncurry f)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith' f xs ys = with' f (zip' xs ys)
zipWith' f xs ys = with' f $ zip' xs ys

dingy :: String -> String
dingy = reverse . tail . reverse . tail

wingy :: [a -> Bool] -> a -> Bool
wingy [] _ = True
wingy (f:fs) x = f x && wingy fs x

wingy' :: [a -> Bool] -> a -> Bool
wingy' fs x = all ($ x) fs
