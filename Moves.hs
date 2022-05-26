module Checkers.Moves where
import Checkers.Types
import Checkers.Types (GameState(GameState))
import Data.Bool (bool)

-- Implement your code for moves function below
moves:: GameState -> SMorJM [Move]
moves x | (jump_moves x /=[]) = JM (jump_moves x)
        | (jump_moves x == []) && (simple_moves x /= []) = SM (simple_moves x)
        | (jump_moves x == []) && (simple_moves x == []) = EndM

--at this point, if I dont get some sort of pity marks for having over 100 submissions
--into gradescope and still not making 18/18, i will cry myself to sleep
jump_moves::GameState -> [Move] 
jump_moves st | status st == RedPlayer = (jumpPiece(redPieces st) st)++(jumpKing(redKings st) st)
              | status st == BlackPlayer = (jumpKing(blackKings st)st)++(jumpPiece(blackPieces st)st)
              | otherwise = []
                where
                    jumpPiece :: PieceState -> GameState -> [Move]
                    jumpPiece xs st = [P(x,y):ys | (x,y)<- xs, ys <- jumpPiece' (x,y)[](x,y) st]

                    jumpPiece' :: Coord -> [Coord] -> Coord -> GameState->[Move]
                    jumpPiece' start rem (x,y) st = [(if(y'' == 0 || y'' == 7) then (K(x'', y''):ys) else (P(x'', y''):ys))
                                                 | ((x', y'),(x'',y''))<-let y' = y+dir(status st)
                                                                             y'' = y+dir'(status st) in [((x+1, y'),(x+2,y'')),((x-1,y'),(x-2,y''))]
                                                  ,not((x',y')`elem` rem) && opponent_occupied (x',y') st && (start==(x'',y'') || notoccupied (x'',y'') st) 
                                                  && onboard (x'',y''),ys <- jump_over (if (y'' == 0) || (y'' == 7) 
                                                  then (jumpKing' start ((x',y'):rem) (x'',y'') st) else (jumpPiece' start ((x',y'):rem) (x'',y'') st))]

                    jumpKing :: PieceState -> GameState -> [Move]                  
                    jumpKing xs st= [K(x,y):ys | (x,y) <- xs, ys <- jumpKing' (x,y) [] (x,y) st]

                    jumpKing' :: Coord -> [Coord] -> Coord -> GameState->[Move]
                    jumpKing' start rem (x,y) st= [K(x'',y''):ys
                                                |((x',y'),(x'',y'')) <- [((x+1,y+1),(x+2,y+2)),((x-1,y+1),(x-2,y+2)),((x+1,y-1),(x+2,y-2)),((x-1,y-1),(x-2,y-2))] -- x' is what its jumping over
                                                , not((x',y') `elem` rem) && opponent_occupied (x',y') st && (start==(x'',y'') || notoccupied (x'',y'') st) 
                                                && onboard (x'',y'') , ys <- jump_over (jumpKing' start ((x',y'):rem) (x'',y'') st)]

                    jump_over [] = [[]]
                    jump_over z = z

simple_moves ::GameState -> [Move]
simple_moves st | status st == RedPlayer = (simpleKing(redKings st)) ++ simplePiece (redPieces st)
                | status st == BlackPlayer = (simpleKing(blackKings st)) ++ simplePiece(blackPieces st)
                | otherwise = []
                    where
                        simplePiece::PieceState->[Move]
                        simplePiece xs = [[P(x, y), (if (y'==0||y'==7) then K(x', y') else P(x',y'))] --gives back coordinates of where simple piece can move
                                         | (x,y) <- xs --puts the coordinates of all simple pieces into (x,y)'s
                                         , (x',y') <- let y' = y + dir(status st) in [(x+1,y'),(x-1,y')] 
                                         , notoccupied (x',y') st && onboard (x',y')]
                        
                        simpleKing :: PieceState -> [Move]
                        simpleKing xs = [[K(x,y),K(x',y')]
                                        | (x,y) <- xs
                                        , (x', y')<- [(x+1,y-1),(x+1,y+1),(x-1,y-1),(x-1,y+1)]
                                        ,notoccupied(x',y') st && onboard (x', y')&& repeatedState([K(x,y),K(x',y')]:(history st))] 

cmpRep :: [Move]->[Move]
cmpRep [] = []
cmpRep [[m,n]] = [[m,n]]
cmpRep ([a,b]:[c,d]:rest) | b == c = [[b,c]]++cmpRep([a,d]:rest)
                          | otherwise = cmpRep([a,b]:rest)++[[c,d]]

detectCycle :: [Move]-> Int->[Int] --checks if theres any cycles at any point and adds it to list
detectCycle [] x = [x]
detectCycle [[a,b]] x | a == b = [x+1]
                      | otherwise = [x]
detectCycle ([a,b]:rest) x = (detectCycle[[a,b]] x)++(detectCycle(rest) x)

sumList :: [Int] -> Int -- sums up list
sumList [] = 0
sumList (x:xs) = x + sumList xs

seeIfMoved :: Int->Bool --checks for move ments at every point
seeIfMoved m | m == 0 =  False
             | m > 0 = True

checker :: (Bool,Bool)->Bool --if return empty for both, its false, otherwise full
checker (x,y) | x && y = False
              | otherwise = True

alternator :: [Move] -> ([Move], [Move])
alternator list1 = go list1 ([],[])
                where
                -- use int i to control the order of which list it will enter
                go [](a,b) = (a,b)
                go (x:xs)(a,b) = go xs (cmpRep b, cmpRep(a++[x]))

applytoBoth ::  ([Move],[Move]) -> (Bool)
applytoBoth (a,b) = checker((seeIfMoved(sumList(detectCycle a 0))),(seeIfMoved(sumList(detectCycle b 0))))

tFst :: [Move] -> [Move]--filters out only simple king moves
tFst([K s,K s']:rest) = [K s,K s']:tFst rest
tFst _ = []

repeatedState :: [Move]-> Bool
repeatedState mv  = (applytoBoth(alternator(tFst(mv))))

opponent_occupied :: Coord -> GameState -> Bool 
opponent_occupied point st
                  |status st == RedPlayer = (point `elem` (blackPieces st)) || (point `elem` (blackKings st))
                  |status st == BlackPlayer = (point `elem` (redPieces st)) || (point `elem` (redKings st)) 

notoccupied :: Coord->GameState->Bool
notoccupied (x,y) st
            | (x,y) `elem` redPieces st = False --if (x,y) are in the redPieces coordinates, it'll say false
            | (x,y) `elem` redKings st = False  --if (x,y) are in redKings coordinates, itll say false
            | (x,y) `elem` blackPieces st = False
            | (x,y) `elem` blackKings st = False
            | otherwise = True

dir ::Status -> Int   --for y'
dir s |s == RedPlayer = -1
      | otherwise = 1

dir' :: Status -> Int
dir' s |s == RedPlayer = -2
       | otherwise = 2

onboard :: Coord->Bool --checks if pieces on board
onboard (x,y) | ((x>=0) && (x<=7) && (y>=0) && (y<=7)) = True
              | otherwise = False
