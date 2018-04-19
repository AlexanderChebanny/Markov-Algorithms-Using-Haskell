module MarkovAlgorithms where

import Data.Text as T (Text, pack, unpack, length, null, drop, append, breakOn)
import Data.Char
import Text.ParserCombinators.ReadP
import Control.Applicative
import System.Environment

{-
    In theoretical computer science, a Markov algorithm is a string rewriting system
    that uses grammar-like rules to operate on strings of symbols.
    Markov algorithms have been shown to be Turing-complete, which means that they
    are suitable as a general model of computation and can represent any mathematical
    expression from its simple notation.
    Markov algorithms are named after the Soviet mathematician Andrey Markov, Jr.
-}

--  Substitution type data
data SubsType = Simple | Final
    deriving (Show, Eq)

-- Substitution data
data Substitution = Sub {
      left::Text
    , right::Text
    , substype::SubsType}
    deriving (Eq)

instance Show Substitution where
    show (Sub left right Simple) = unpack left ++ " -> " ++ unpack right
    show (Sub left right Final) = unpack left ++ " ->. " ++ unpack right

type Algorithm = [Substitution]

-- O(n) complexity where n = max(length str, length left).
-- Applies substitution to the string. Returns Nothing if nothing is replaced.
applySubstitution :: Substitution -> Text -> Maybe Text
applySubstitution (Sub left right t) str
    | T.null left = Just (right `append` str)
    | not $ T.null post
        = Just ((pre `append` right) `append` (T.drop (T.length left) post))
    | otherwise = Nothing
    where
        (pre, post) = breakOn left str

data Either3 a = Continue a | Fin a | Inapplicable a

step :: Algorithm -> Text -> Either3 Text
step [] str = Inapplicable str
step (x@(Sub _ _ t):xs) str =
    case applySubstitution x str of
        Just s ->
            case t of
                Simple -> Continue s
                Final -> Fin s
        Nothing -> step xs str

-- Applies Markov Algorithm to the string.
applyAlgorithm :: Algorithm -> Text -> [Text]
applyAlgorithm alg string = string : helper string -- False
    where
    helper str =
        case step alg str of
            Continue s -> s : helper s
            Fin s -> [s]
            Inapplicable s -> []
            {- if f then [] else [s] -}

-- Additional version
applyAlgorithmStr :: Algorithm -> String -> [String]
applyAlgorithmStr alg string =
    map unpack (applyAlgorithm alg (pack string))

algorithmResult :: Algorithm -> String -> String
algorithmResult = \alg -> last . applyAlgorithmStr alg

-- Unary Integer Division Markov Algorithm
divisionAlg = [Sub (pack "*11") (pack "1*") Simple, Sub (pack "*1") (pack "#1") Final,
    Sub (pack "*") (pack "#") Final, Sub (pack "") (pack "*") Simple]

-- Palindrome Markov Algorithm. Alphabet = {a, b}
paliAlg = [Sub (pack "*a") (pack "A") Simple, Sub (pack "*b") (pack "B") Simple, Sub (pack "Aa") (pack "aA") Simple, Sub (pack "Ab") (pack "bA") Simple,
    Sub (pack "Ba") (pack "aB") Simple,Sub (pack "Bb") (pack "bB") Simple,Sub (pack "aA") (pack "") Simple,Sub (pack "bB") (pack "") Simple,
    Sub (pack "aB") (pack "a") Final,Sub (pack "bA") (pack "b") Final,Sub (pack "A") (pack "") Final,Sub (pack "B") (pack "") Final,
    Sub (pack "*") (pack "") Final,Sub (pack "") (pack "*") Simple]

string' :: String -> ReadP String
string' = traverse (satisfy . (==))

isInAlphabet :: String -> Char -> Bool
isInAlphabet alphabet char = any (==char) alphabet

inAlphabet :: String -> ReadP Char
inAlphabet alphabet = satisfy $ isInAlphabet alphabet

sLeft :: String -> ReadP String
sLeft alphabet = do
    left <- many1 (inAlphabet alphabet) <|> string ""
    string " ->" <|> string "->"
    return left

sType :: ReadP SubsType
sType = do
    t <- string' ". " <|> string " " <|> string "." <|> string ""
    case t of
        " " -> return Simple
        ". " -> return Final
        "." -> return Final
        "" -> return Simple
        _ -> error "Unknown Error!"

alphabetWord :: String -> ReadP String
alphabetWord alphabet = many1 (inAlphabet alphabet) <|> string ""

-- Additional function for main
isAlphabetWord :: String -> String -> Bool
isAlphabetWord alphabet word =
    case readP_to_S (alphabetWord alphabet) word of
        [] -> False
        x -> case last x of
                (_, "") -> True
                _ -> False

substitutionParser :: String -> ReadP Substitution
substitutionParser alphabet = do
    left <- sLeft alphabet
    t <- sType
    right <- alphabetWord alphabet
    return $ Sub (pack left) (pack right) t

parseSubstitution :: String -> String -> Maybe Substitution
parseSubstitution alphabet input =
    case readP_to_S (substitutionParser alphabet) input of
        [] -> Nothing
        x -> case last x of
                (result, "") -> Just result
                _ -> Nothing

parseAlphabetAndAlgorithm :: String -> (String, Algorithm)
parseAlphabetAndAlgorithm input =
    case (filter (not . Prelude.null) . lines) input of
        [] -> error "Wrong input"
        (x:[]) -> error "Wrong input"
        (alphabet:algorithm) -> (alphabet, parseAlgorithm alphabet algorithm)
    where
        parseAlgorithm :: String -> [String] -> Algorithm
        parseAlgorithm _ [] = []
        parseAlgorithm alphabet (x:xs) =
            case parseSubstitution alphabet x of
                Just s -> s : parseAlgorithm alphabet xs
                Nothing -> error "Wrong input"

main :: IO()
main = do
    (filename:mode:_) <- getArgs
    (alphabet, algorithm) <- fmap parseAlphabetAndAlgorithm $ readFile filename
    putStrLn $ "Алфавит = {" ++ alphabet ++ "}"
    -- print algorithm
    loop algorithm mode alphabet
    where
        loop algorithm mode alphabet = do
            -- putStrLn $ "Input: "
            word <- getLine
            if (map toLower word) == "exit"
                then return ()
                else do
                    case (mode, isAlphabetWord alphabet word) of
                        (_, False) -> putStrLn $
                            "Строка не является словом алфавита, повторите ввод"
                        ("res", _) -> putStrLn $ "Result = " ++
                                    algorithmResult algorithm word
                        ("comp",_) -> putStr "Result = " >>
                            print (applyAlgorithmStr algorithm word)
                        _ -> error "Wrong mode"
                    loop algorithm mode alphabet
