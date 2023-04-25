module Ploy where  -- do NOT CHANGE export of module



-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Char
import Data.Bits ( (.&.), (.|.), shift, popCount, testBit )
import Board ( line, Player(..), Cell(..), Board, Pos(Pos) )
import Data.Char(ord, chr)



-- #############################################################################
-- ########################### GIVEN IMPLEMENTATION ############################
-- #############################################################################

data Move = Move {start :: Pos, target :: Pos, turn :: Int}

instance Show Move where
  show (Move (Pos startC startR) (Pos tarC tarR) tr) = [startC] ++ (show startR) ++ "-" ++ [tarC] ++ show tarR ++ "-" ++ show tr

instance Eq Move where
  (==) (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) (Move (Pos sc2 sr2) (Pos tc2 tr2) r2) =
      sc1 == sc2 && sr1 == sr2 && tc1 == tc2 && tr1 == tr2 && r1 == r2 

rotate :: Int -> Int -> Int
rotate o tr = (.&.) ((.|.) (shift o tr) (shift o (tr-8))) 255

-- #############################################################################
-- ####################### gameFinished :: Board -> Bool #######################
-- ####################### - 3 Implementation Points     #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################

gameFinished :: Board -> Bool
gameFinished s = (checkCommander s 0 0)  == True || (checkPlayers s True True ) == True

checkCommander :: Board -> Int -> Int -> Bool
checkCommander _ 1 1 = False
checkCommander (((Piece Black _): xs):y) 1 b2 = checkCommander (xs:y) 1 b2
checkCommander (((Piece White _): xs):y) b1 1 = checkCommander (xs:y) b1 1
checkCommander (((Piece Black x): xs):y) b1 b2 = if  ((x==170) || (x == 85)) then checkCommander (xs:y) (b1+1) b2  else checkCommander (xs:y) b1 b2
checkCommander (((Piece White x): xs):y) b1 b2 = if  ((x==170) || (x == 85))  then checkCommander (xs:y) b1 (b2+1)  else checkCommander (xs:y) b1 b2
checkCommander (((Empty): xs):y) b1 b2 = checkCommander (xs:y) b1 b2
checkCommander ([]:xss) b1 b2 = checkCommander xss b1 b2
checkCommander [] _ _= True

-- SCHAUT SICH IRGENDWIE NUR EINE FARBE AN

checkPlayers :: Board -> Bool -> Bool -> Bool
checkPlayers _ False False = False
checkPlayers (((Piece Black x): xs):y) b1 b2 = if  ((x==170) || (x == 85))  then checkPlayers (xs:y) b1 b2 else checkPlayers (xs:y) False b2 
checkPlayers (((Piece White x): xs):y) b1 b2 = if  ((x==170) || (x == 85)) then checkPlayers (xs:y) b1 b2 else  checkPlayers (xs:y) b1 False 
checkPlayers (((Empty): xs):y) b1 b2 = checkPlayers (xs:y) b1 b2 
checkPlayers ([]:xss) b1 b2 = checkPlayers xss b1 b2
checkPlayers [] _ _= True




-- #############################################################################
-- ################### isValidMove :: Board -> Move -> Bool ####################
-- ################### - 5 Implementation Points            ####################
-- ################### - 1 Coverage Point                   ####################
-- #############################################################################


isValidMove :: Board -> Move -> Bool
--isValidMove _ (Move (Pos a b) (Pos c d) x ) = if (x>=1) && (getLength (convert2 (Move (Pos a b) (Pos c d) x )) >= 1) then False else 
isValidMove z (Move (Pos c1 r1) (Pos c2 r2) x ) 
          | (x >= 1 && x <= 7 &&((z!!(9-r1))!!(ord c1 -97) /= Empty)  && c1 == c2 && r1 == r2) = True 
          | ((z!!(9-r1))!!(ord c1 -97) == Empty) = False  -- erste Pos ist empty
          | x < 0 || x >=8 = False                        -- mehr als 7 Drehungen
          | x ==0 && ((z!!(9-r1))!!(ord c1 -97) /= Empty) &&  (z!!(9-r2))!!(ord c2 -97) == Empty = lastIsEmpty z (Move (Pos c1 r1) (Pos c2 r2) x )
          | x ==0 && ((z!!(9-r1))!!(ord c1 -97) /= Empty) &&  (z!!(9-r2))!!(ord c2 -97) /= Empty = lastIsNotEmpty z (Move (Pos c1 r1) (Pos c2 r2) x )
          | x >0 && (getLength( betweenPos(convert2 (Move (Pos c1 r1) (Pos c2 r2) x ))) >= 1) = False
          | x >0 && (getLength( betweenPos(convert2 (Move (Pos c1 r1) (Pos c2 r2) x ))) <= 1) &&  ((z!!(9-r1))!!(ord c1 -97) /= Empty) &&  (z!!(9-r2))!!(ord c2 -97) == Empty = lastIsEmpty z (Move (Pos c1 r1) (Pos c2 r2) x )
          | x >0 && (getLength( betweenPos(convert2 (Move (Pos c1 r1) (Pos c2 r2) x ))) <= 1) &&  ((z!!(9-r1))!!(ord c1 -97) /= Empty) &&  (z!!(9-r2))!!(ord c2 -97) /= Empty = lastIsNotEmpty z (Move (Pos c1 r1) (Pos c2 r2) x )
          
                                             
-- (((z!!(9-r1))!!(ord c1 -97) /= Empty) && x >= 1 && x <= 7 && abs(ord c1 - ord c2) >= 0  && abs(ord c1 - ord c2) <=1 && abs (r1-r2) <= 1 && abs (r1-r2) >= 0) = True 

lastIsEmpty :: Board -> Move -> Bool
lastIsEmpty b m  =  if(cleanLine b m == True && outOfBoard m == False) then True else False 

lastIsNotEmpty :: Board -> Move -> Bool
lastIsNotEmpty b m = if(cleanLine b m == True &&  notSameColor b m == True && outOfBoard m == False) then True else False 



isEmpty :: Board -> [Pos] -> Bool     -- aufrufen auf die Liste betweenPos (ohne erster und letzer Wert) (alle Werte zwischen Start und Ende sind leer)
isEmpty _ [] = True
isEmpty b ((Pos x y):xs) = if (b!!(9-y))!!(ord x -97) == Empty then isEmpty b xs else False


betweenPos::[a]->[a]
betweenPos [] = []
betweenPos xs = tail (init xs)


takePlayer :: Cell -> Player
takePlayer  (Piece White _) = White
takePlayer (Piece Black _) = Black


convert2 :: Move -> [Pos]
convert2  (Move (Pos c1 r1) (Pos c2 r2) _) = line (Pos c1 r1) (Pos c2 r2)
-------------------

cleanLine :: Board -> Move -> Bool
cleanLine b m | (betweenPos(convert2 m )) == []  = True  
              |  (isEmpty b (betweenPos(convert2 m )) == True) = True 
              | otherwise = False
 
outOfBoard :: Move -> Bool
outOfBoard (Move(Pos _ _) (Pos c2 r2) _) = if ((ord c2) >=97 && (ord c2) <= 105 && r2 <= 9 && r2 >=1) then False else True


notSameColor :: Board -> Move -> Bool    -- Auf der Zielposition ist, wenn dann, eine gegnerische Farbe, oder Empty
notSameColor b (Move (Pos x1 y1) (Pos x2 y2) _ ) = if takePlayer((b!!(9-y1))!!(ord x1 -97)) /= takePlayer((b!!(9-y2))!!(ord x2 -97)) then True else False

getLength :: [a] -> Int
getLength [] = 0
getLength (_:xs) = 1 + getLength xs


-- #############################################################################
-- ################### possibleMoves :: Pos -> Cell -> [Move] ##################
-- ################### - 6 Implementation Points              ##################
-- ################### - 1 Coverage Point                     ##################
-- #############################################################################

possibleMoves :: Pos -> Cell -> [Move]
possibleMoves _ Empty = []
possibleMoves (Pos a b) (Piece _ y) | popCount y == 1 = filter checkMove ([moveShield (Pos a b) y] ++ drehen (Move (Pos a b)(Pos a b) 0) ++ drehen (moveShield (Pos a b )y))
                                    | popCount y == 2 = if (testBit y 0 && testBit y 4 || testBit y 1 && testBit y 5 || testBit y 2 && testBit y 6 || testBit y 3 && testBit y 7) then filter checkMove (moveProbe2 (Pos a b) y 0  ++ drehProbe (Move (Pos a b)(Pos a b) 0))  else filter checkMove  (moveProbe2 (Pos a b) y 0  ++ drehen (Move (Pos a b)(Pos a b) 0) )
                                    | popCount y == 3 = filter checkMove (moveLance2 (Pos a b) y 0  ++ drehen (Move (Pos a b)(Pos a b) 0))
                                    | popCount y == 4 = filter checkMove (moveCommander (Pos a b) y 0  ++ drehCommander (Move (Pos a b)(Pos a b) 0)) 
                                    
                                                                           

obenRechts :: Pos -> Int -> Move            
obenRechts (Pos a b) i = (Move (Pos a b) (Pos (chr((ord a) + i)) (b+i)) 0) 

untenRechts :: Pos -> Int -> Move            
untenRechts (Pos a b) i = (Move (Pos a b) (Pos (chr((ord a) + i)) (b-i)) 0) 

rechts :: Pos -> Int -> Move            
rechts (Pos a b) i = (Move (Pos a b) (Pos (chr((ord a) + i)) b) 0) 

links :: Pos -> Int -> Move                                              -- unten = b-1   -- links = a -1
links (Pos a b) i = (Move (Pos a b) (Pos (chr((ord a) - i)) b) 0) 

obenlinks :: Pos -> Int -> Move            
obenlinks (Pos a b) i = (Move (Pos a b) (Pos (chr((ord a) - i)) (b+i)) 0) 

untenlinks :: Pos -> Int -> Move            
untenlinks (Pos a b) i = (Move (Pos a b) (Pos (chr((ord a) - i)) (b-i)) 0) 

oben :: Pos -> Int -> Move            
oben (Pos a b) i = (Move (Pos a b) (Pos a (b+i)) 0 )

unten :: Pos -> Int -> Move            
unten (Pos a b) i = (Move (Pos a b) (Pos  a (b-i)) 0) 

moveShield :: Pos -> Int -> Move
moveShield  p i  | testBit i 0  = oben p 1
              | testBit i 1 = obenRechts p 1
              | testBit i 2 = rechts  p 1
              | testBit i 3 = untenRechts p 1
              | testBit i 4 = unten p 1
              | testBit i 5 = untenlinks p 1
              | testBit i 6 = links p 1
              | testBit i 7 = obenlinks p 1


moveCommander :: Pos -> Int -> Int -> [Move]  
moveCommander p i 0 = if testBit i 0 then  merge [oben p 1]  (moveCommander p i 1) else (moveCommander p i 1)
moveCommander p i 1 = if testBit i 1 then  merge [obenRechts p 1]  (moveCommander p i 2) else (moveCommander p i 2)
moveCommander p i 2 = if testBit i 2 then  merge [rechts p 1]  (moveCommander p i 3) else (moveCommander p i 3)
moveCommander p i 3 = if testBit i 3 then  merge [untenRechts p 1]  (moveCommander p i 4) else (moveCommander p i 4)
moveCommander p i 4 = if testBit i 4 then  merge [unten p 1]  (moveCommander p i 5) else (moveCommander p i 5)
moveCommander p i 5 = if testBit i 5 then  merge [untenlinks p 1]  (moveCommander p i 6) else (moveCommander p i 6)
moveCommander p i 6 = if testBit i 6 then  merge [links p 1]  (moveCommander p i 7) else (moveCommander p i 7)
moveCommander p i 7 = if testBit i 7 then  merge [obenlinks p 1]  [] else []



moveProbe2 :: Pos -> Int -> Int -> [Move] 
moveProbe2  p i 0 = if testBit i 0 then  merge (merge [oben p 1] [oben p 2]) (moveProbe2 p i 1)  else (moveProbe2 p i 1)       
moveProbe2  p i 1 = if testBit i 1 then  merge (merge [obenRechts p 1] [obenRechts p 2]) (moveProbe2 p i 2)  else (moveProbe2 p i 2) 
moveProbe2  p i 2 = if testBit i 2 then  merge (merge [rechts p 1] [rechts p 2]) (moveProbe2 p i 3)  else (moveProbe2 p i 3)
moveProbe2  p i 3 = if testBit i 3 then  merge (merge [untenRechts p 1] [untenRechts p 2]) (moveProbe2 p i 4)  else (moveProbe2 p i 4)
moveProbe2  p i 4 = if testBit i 4 then  merge (merge [unten p 1] [unten p 2]) (moveProbe2 p i 5)  else (moveProbe2 p i 5)
moveProbe2  p i 5 = if testBit i 5 then  merge (merge [untenlinks p 1] [untenlinks p 2]) (moveProbe2 p i 6)  else (moveProbe2 p i 6)
moveProbe2  p i 6 = if testBit i 6 then  merge (merge [links p 1] [links p 2]) (moveProbe2 p i 7)  else (moveProbe2 p i 7)
moveProbe2  p i 7 = if testBit i 7 then  merge (merge [obenlinks p 1] [obenlinks p 2]) []  else []


                
moveLance2 :: Pos -> Int -> Int -> [Move] 
moveLance2 p i 0 =  if testBit i 0 then  merge (merge (merge [oben p 1] [oben p 2]) [oben p 3]) (moveLance2 p i 1) else  (moveLance2 p i 1)    
moveLance2 p i 1 =  if testBit i 1 then  merge (merge (merge [obenRechts p 1] [obenRechts p 2]) [obenRechts p 3]) (moveLance2 p i 2) else  (moveLance2 p i 2) 
moveLance2 p i 2 =  if testBit i 2 then  merge (merge (merge [rechts p 1] [rechts p 2]) [rechts p 3]) (moveLance2 p i 3) else  (moveLance2 p i 3) 
moveLance2 p i 3 =  if testBit i 3 then  merge (merge (merge [untenRechts p 1] [untenRechts p 2]) [untenRechts p 3]) (moveLance2 p i 4) else  (moveLance2 p i 4) 
moveLance2 p i 4 =  if testBit i 4 then  merge (merge (merge [unten p 1] [unten p 2]) [unten p 3]) (moveLance2 p i 5) else  (moveLance2 p i 5) 
moveLance2 p i 5 =  if testBit i 5 then  merge (merge (merge [untenlinks p 1] [untenlinks p 2]) [untenlinks p 3]) (moveLance2 p i 6) else  (moveLance2 p i 6) 
moveLance2 p i 6 =  if testBit i 6 then  merge (merge (merge [links p 1] [links p 2]) [links p 3]) (moveLance2 p i 7) else  (moveLance2 p i 7) 
moveLance2 p i 7 =  if testBit i 7 then  merge (merge (merge [obenlinks p 1] [obenlinks p 2]) [obenlinks p 3]) [] else  []                     
       

drehen :: Move ->  [Move]
drehen (Move p1 p2 _) = [(Move p1 p2 1)] ++ [(Move p1 p2 2)] ++ [(Move p1 p2 3)] ++ [(Move p1 p2 4)] ++ [(Move p1 p2 5)] ++ [(Move p1 p2 6)] ++ [(Move p1 p2 7)] 

drehCommander :: Move -> [Move]
drehCommander (Move p1 p2 _) = [(Move p1 p2 1)]  ++ [(Move p1 p2 3)] ++ [(Move p1 p2 5)]  ++ [(Move p1 p2 7)] 

drehProbe :: Move -> [Move]
drehProbe  (Move p1 p2 _) = [(Move p1 p2 1)] ++ [(Move p1 p2 2)] ++ [(Move p1 p2 3)] ++ [(Move p1 p2 5)] ++ [(Move p1 p2 6)] ++ [(Move p1 p2 7)] 


merge :: [a] -> [a] -> [a]
merge s  []     = s
merge []  s     = s
merge (x:xs) (y:ys) = x : y : merge xs ys

checkMove :: Move -> Bool
checkMove (Move (Pos c1 r1) (Pos c2 r2) _) = if ord c1 < 97 || ord c1 > 105 || ord c2 < 97 || ord c2 > 105 || r1 >9 || r1 <= 0 || r2 > 9 || r2 <= 0 then False else True

             
-- ############################################################################
-- ############# IMPLEMENT listMoves :: Board -> Player -> [Move] ##############
-- ############# - 2 Implementation Points                        ##############
-- ############# - 1 Coverage Point                               ##############
-- #############################################################################

listMoves :: Board -> Player -> [Move]
listMoves  (((x): xs):y) p 
              | gameFinished (((x): xs):y) = []
              | otherwise = onlyValids (((x): xs):y) (iterater (((x): xs):y) p (Pos 'a' 9))
              

onlyValids:: Board -> [Move] -> [Move]
onlyValids b (x:xs) = if (isValidMove b x) == False then [] ++ (onlyValids b xs) else [x] ++ (onlyValids b xs) 
onlyValids _ [] = []
  -- 144

notNeeded :: Cell -> Player -> Bool
notNeeded (Empty) _ = True
notNeeded (Piece p _ ) p1 = if (p==p1) then False else True


iterater :: Board -> Player -> Pos -> [Move]
iterater [] _ _ = []
iterater ([]:y) p (Pos _ r) = iterater y p (Pos 'a' (r-1)) -- neue Zeile
iterater (((x): xs):y) p (Pos c r) = if (notNeeded x p) then [] ++ (iterater (xs:y) p (Pos (chr((ord c) + 1)) r)) else (possibleMoves (Pos (c) r) x) ++ (iterater (xs:y) p (Pos (chr((ord c) + 1)) r))

              
------------------------------------------------------------------


