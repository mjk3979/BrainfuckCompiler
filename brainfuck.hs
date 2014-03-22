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
optIncDec [] c = if c == 0 then return () else (putStr "add byte [ecx], ") >> putStrLn (show c)
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
			--Main.IfLeft(inside, _) -> contin ((putStrLn "while (*dp) {") >> (evalBrainFuck inside) >> (putStr "}"))
			Main.Left -> contin (putStrLn "sub ecx, 1")
			Main.Right -> contin (putStrLn "add ecx, 1")
			Print -> contin (putStrLn "mov edx, 1\nmov ebx, 1\nmov eax, 4\nint 0x80")
			--Read -> contin (putStrLn "*dp=getchar();")


main = do
	allData <- getContents
	let 
		symbols = matchIfs (map (\c -> charToSym c) (filter Main.isSymbol allData)) [] []

	(putStrLn "section .bss\narr resb 10000\nsection .text\nglobal _start\n_start:\nmov ecx, arr") >> (evalBrainFuck symbols) >> (putStrLn "mov eax, 1\nmov ebx, 0\nint 0x80")
