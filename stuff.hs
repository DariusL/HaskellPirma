data Cont a = Cont a deriving (Show)

instance Monad Cont where
	return a = Cont a
	(Cont m) >>= f = f m

conting :: Bool -> Cont Bool
conting b = Cont $ not b

data Pair a = Pair a Int deriving Show

instance Monad Pair where
	return x = Pair x 0
	(Pair x c) >>= f = let Pair x' _ = f x in Pair x' c

stuff :: String -> Pair String
stuff s = Pair (s ++ "!") 0

data Magic a = Magic a Bool deriving Show

instance Monad Magic where
	return x = Magic x True
	(Magic x True) >>= f = let Magic x' _ = f x in Magic x' False
	(Magic x False) >>= f = let Magic x' _ = f x in Magic x' True

magicka :: Int -> Magic Int
magicka a = Magic (a + 10) False