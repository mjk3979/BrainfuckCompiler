import Data.Char

data Symbol = Right
			| Left
			| Inc
			| Dec
			| Read
			| Print
			| IfLeft ([Symbol], Bool)
			| IfRight
	deriving Show

charToSym :: Char -> Symbol
charToSym c = 
	case c of
		'>' -> Main.Right
		'<' -> Main.Left
		'+' -> Inc
		'-' -> Dec
		',' -> Read
		'.' -> Print
		'[' -> IfLeft ([], False)
		']' -> IfRight

matchIfs :: [Symbol] -> [Symbol] -> [[Symbol]] -> [Symbol]
matchIfs [] acc [] = acc
matchIfs (h:t) acc stack = 
	let
		f add newstack = 
			case newstack of
				[] -> matchIfs t (acc ++ [add]) newstack
				otherwise -> matchIfs t acc ((head newstack ++ [add]) : (tail newstack))
	in
		case h of
			IfLeft _ -> matchIfs t acc ([]:stack)
			IfRight -> f (IfLeft ((head stack), False)) (tail stack)
			otherwise -> f h stack
		
isSymbol :: Char -> Bool
isSymbol c = 
	case c of
		'>' -> True
		'<' -> True
		'+' -> True
		'-' -> True
		',' -> True
		'.' -> True
		'[' -> True
		']' -> True
		otherwise -> False

modify :: Int -> (a -> a) -> [a] -> [a]
modify i f l = h ++ [f x] ++ t
	where (h, x:t) = splitAt i l

clipNum :: Int -> Int
clipNum x = ((x `mod` 256) + 256) `mod` 256

evalBrainFuck :: [Symbol] -> [Int] -> Int -> String -> String -> Integer -> String
evalBrainFuck [] _ _ _ acc ops = acc
evalBrainFuck (h:t) d p inp acc ops = if ops >= 100000 then acc ++ "\nPROCESS TIME OUT. KILLED!!!" else
	case h of
		Main.Right -> evalBrainFuck t d (p+1) inp acc (ops+1)
		Main.Left -> evalBrainFuck t d (p-1) inp acc (ops+1)
		Inc -> evalBrainFuck t (modify p (\x -> clipNum (x + 1)) d) p inp acc (ops+1)
		Dec -> evalBrainFuck t (modify p (\x -> clipNum (x - 1)) d) p inp acc (ops+1)
		Read -> evalBrainFuck t (modify p (\x -> ord (head inp)) d) p (tail inp) acc (ops+1)
		Print -> evalBrainFuck t d p inp (acc ++ [chr (d !! p)]) (ops+1)
		IfLeft (inside, notFirst) ->
			(case d !! p of
				0 -> evalBrainFuck t d p inp acc ( ops + ( if notFirst then 1 else 2))
				otherwise -> evalBrainFuck (inside ++ [IfLeft (inside, True)] ++ t) d p inp acc ( ops + (if notFirst then 2 else 1))
				)


main = do
	allData <- getContents
	let 
		ls = lines allData
		nm = map (\s -> (read s) :: Int) (words (head ls))
		numInput = head nm
		numLines = head (tail nm)
		inp = take numInput (ls !! 1)
		symbols = matchIfs (map (\c -> charToSym c) (filter Main.isSymbol (foldl (++) [] (take numLines (tail (tail ls)))))) [] []

	putStrLn (evalBrainFuck symbols (repeat (0 :: Int)) 0 inp "" 0)
