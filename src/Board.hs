module Board where  -- do NOT CHANGE export of module

import Data.Char(isDigit, ord, chr)
import Data.List.Split ( splitOn )
-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Chars

-- #############################################################################
-- ############# GIVEN IMPLEMENTATION                           ################
-- ############# Note: "deriving Show" may be deleted if needed ################
-- #############       Given data types may NOT be changed      ################
-- #############################################################################

data Player = Black | White deriving Show
data Cell = Piece Player Int | Empty deriving Show     -- (Piece c i)
data Pos = Pos { col :: Char, row :: Int } deriving Show
type Board = [[Cell]]

instance Eq Pos where
  (==) (Pos c1 r1) (Pos c2 r2) = (c1 == c2) && (r1 == r2)

instance Eq Player where
  (==) Black Black = True
  (==) White White = True
  (==) _ _ = False

instance Eq Cell where
  (==) Empty Empty = True
  (==) (Piece p1 i1) (Piece p2 i2) = p1 == p2 && i1 == i2 
  (==) _ _ = False

-- #############################################################################
-- ################# IMPLEMENT validateFEN :: String -> Bool ###################
-- ################## - 2 Implementation Points              ###################
-- ################## - 1 Coverage Point                     ###################
-- #############################################################################


validateFEN :: String -> Bool
validateFEN [] = False
validateFEN s = if countSlash s 0 0 == True && checkChar s == True then True else False


countSlash :: String -> Int -> Int -> Bool
countSlash _ 9 _ = False
countSlash _ _ 9 = False         -- i countet die Kommas, j countet die Slashes. Wenn ein Slash kommt, setzen wir KommaZählung wieder auf 0.
countSlash [] 8 8 = True
countSlash (',':xs) i j = countSlash xs (i +1) j 
countSlash ('/':xs) i j = if i == 8 then countSlash xs 0 (j + 1) else False
countSlash (x:xs) i j = if x == 'b' || x == 'w' || isDigit x == True then countSlash xs i j else False
countSlash [] i j = if i /= 8 ||  j /= 8 then False else True  
--countSlash (x:y:xs) i j = if x == 'b' || x == 'w' then if  isDigit y == True  then countSlash (y:xs) i j else False else False

stringToInt :: String -> String -> Int
stringToInt ('/':_) [] = -1 
stringToInt (',':_) [] = -1 
stringToInt [][] = -1
stringToInt ('/': _) t = read t :: Int
stringToInt (',': _) t = read t :: Int
stringToInt [] t = read t :: Int
stringToInt (x:xs) t = if isDigit x == True then stringToInt xs  (t ++ [x]) else -1

checkChar :: String -> Bool
checkChar [] = True
checkChar ('b':xs) = if ((stringToInt xs []) >= 1 && (stringToInt xs []) <= 255) then checkChar (takeNum xs) else False
checkChar ('w':xs) = if ((stringToInt xs [])  >=1 && (stringToInt xs [])  <= 225) then checkChar (takeNum xs) else False
checkChar (',': xs) = checkChar xs
checkChar ('/':xs) = checkChar xs  
checkChar _ = False

takeNum :: String -> String 
takeNum (x:xs) = if isDigit x == True then takeNum xs else xs



deleteChar :: String -> String 
deleteChar [] = []
deleteChar (x:xs) = if isDigit x == False then deleteChar xs else x : deleteChar xs 


-- #############################################################################
-- ####################### buildBoard :: String -> Board #######################
-- ####################### - 2 Implementation Points     #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################


buildBoard :: String -> Board
buildBoard  [] = []
buildBoard s = buildHelper2(deleteSlash s) 


chooseCell :: String -> Cell
chooseCell s = if (s == "") then Empty else Piece (if (head s) == 'w' then White else Black) (stringToInt (deleteChar s) [])


buildHelper :: [String] -> [Cell]
buildHelper [] = [] 
buildHelper (x:xs) = chooseCell x : buildHelper xs


buildHelper2 :: [String] -> [[Cell]]
buildHelper2 [] = []
buildHelper2 (x:xs) = buildHelper (deleteComma x) : buildHelper2 xs


deleteSlash :: String -> [String]
deleteSlash s = splitOn "/" s


deleteComma :: String -> [String]
deleteComma s = splitOn "," s
   


-- #############################################################################
-- ####################### line :: Pos -> Pos -> [Pos]  ########################
-- ####################### - 3 Implementation Points    ########################
-- ####################### - 1 Coverage Point           ########################
-- #############################################################################


line :: Pos -> Pos -> [Pos]
line (Pos c1 r1) (Pos c2 r2) = 
 if (ord c1 < ord c2 && r1 == r2) then [(Pos c1 r1)] ++ line (Pos (chr((ord c1) +1)) r1) (Pos c2 r2)
else if (ord c1 < ord c2 && r1 > r2) then [(Pos c1 r1)] ++ line (Pos (chr((ord c1) +1)) (r1-1)) (Pos c2 r2)
else if (ord c1 < ord c2 && r1 < r2) then [(Pos c1 r1)] ++ line (Pos (chr((ord c1) +1)) (r1+1)) (Pos c2 r2)
else if (ord c1 > ord c2 && r1 == r2) then[(Pos c1 r1)] ++ line (Pos (chr((ord c1) -1)) r1) (Pos c2 r2)
else if (ord c1 > ord c2 && r1 > r2) then [(Pos c1 r1)] ++ line (Pos (chr((ord c1) -1)) (r1-1)) (Pos c2 r2)
else if (ord c1 > ord c2 && r1 < r2) then [(Pos c1 r1)] ++  line (Pos (chr((ord c1) -1)) (r1+1)) (Pos c2 r2)
else if (ord c1 == ord c2 && r1 > r2) then [(Pos c1 r1)] ++ line (Pos (chr((ord c1))) (r1-1)) (Pos c2 r2)
else if (ord c1 == ord c2 && r1 < r2) then [(Pos c1 r1)] ++ line (Pos (chr((ord c1))) (r1+1)) (Pos c2 r2)
else  [(Pos c1 r2)] -- Anker





