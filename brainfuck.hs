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

optIncDec :: [Symbol] -> Int -> IO ()
optIncDec [] c = if c == 0 then return () else (putStr "*dp+=") >> putStr (show c) >> putStr ";"
optIncDec (h:t) c = 
	case h of
		Inc -> optIncDec t (clipNum c+1)
		Dec -> optIncDec t (clipNum c-1)
		_ -> (optIncDec [] c) >> evalBrainFuck (h:t)

evalBrainFuck :: [Symbol] -> IO ()
evalBrainFuck [] = return ()
evalBrainFuck (h:t) =
	let
		contin x = (x >> (evalBrainFuck t))
	in 
		case h of
			Inc -> optIncDec t 1
			Dec -> optIncDec t (-1)
			Main.IfLeft(inside, _) -> contin ((putStr "while (*dp) {") >> (evalBrainFuck inside) >> (putStr "}"))
			Main.Left -> contin (putStr "--dp;")
			Main.Right -> contin (putStr "++dp;")
			Print -> contin (putStr "putchar(*dp);")
			Read -> contin (putStr "*dp=getchar();")


main = do
	allData <- getContents
	let 
		symbols = matchIfs (map (\c -> charToSym c) (filter Main.isSymbol allData)) [] []

	(putStr "int main() { char *arr = malloc(10000); char *dp = arr;") >> (evalBrainFuck symbols) >> (putStr "free(arr);return 0;}")
