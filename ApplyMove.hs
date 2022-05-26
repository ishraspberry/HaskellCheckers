module Checkers.ApplyMove where
import Checkers.Moves
import Checkers.Types
------------------------------------------------
------------------------------------------------
--
--  APPLYING A MOVE to a state
--
------------------------------------------------
------------------------------------------------
apply_move:: Move -> GameState -> GameState
apply_move mv st | moves st == EndM = gameStats st    
                 | mv `elem` jump_moves st = make_jump_move mv mv st
                 | (jump_moves st == []) && mv `elem` simple_moves st = make_simple_move mv mv st
                 | otherwise = (st{message = "illegal moves bro"++show(moves st)})

make_simple_move ([K a, K b]) (moveKeep) st
  | status st == RedPlayer && a `elem` redKings st
    = st{ redKings = replace a b (redKings st),  status = BlackPlayer, history = moveKeep:(history st)}
  | status st == BlackPlayer && a `elem` blackKings st
    = st{ blackKings = replace a b (blackKings st),  status = RedPlayer, history = moveKeep:(history st)}

make_simple_move ([P a, P b]) (moveKeep) st
  | status st == RedPlayer && a `elem` redPieces st && (snd b == 7)
    = st{redPieces = remove a (redPieces st),  status = BlackPlayer,  redKings = b:redKings st, history = moveKeep:(history st)}
  | status st == RedPlayer && a `elem` redPieces st && (snd b /= 7)
    = st{redPieces = replace a b (redPieces st),  status = BlackPlayer, history = moveKeep:(history st)}
  | status st == BlackPlayer && a `elem` (blackPieces st) && (snd b == 0)
    = st{blackPieces = remove a (blackPieces st), blackKings = b:blackKings st,  status = RedPlayer, history = moveKeep:(history st)}
  | status st == BlackPlayer && a`elem` (blackPieces st) && (snd b /= 0)
    = st{blackPieces = replace a b (blackPieces st),  status = RedPlayer, history = moveKeep:(history st)}
  
make_jump_move :: Move -> Move -> GameState -> GameState
make_jump_move (x:[]) moveKeep st | status st == RedPlayer = (st{status = BlackPlayer, history=moveKeep:(history st)})
                                  | otherwise = st{status = RedPlayer, history=moveKeep:(history st)}
                                  where
                                    porky x = x

make_jump_move (a:(b:rest)) moveKeep st 
              | status st == RedPlayer && start `elem` (redKings st)
                = make_jump_move (b:rest)(moveKeep)
                 (st{blackKings = remove (mid_jump (start)(next)) (blackKings st)
                 ,blackPieces = remove(mid_jump start next) (blackPieces st)
                 ,redKings = next:(remove start (redKings st))})
                 
              | status st == BlackPlayer && start `elem` (blackKings st)
                = make_jump_move (b:rest)(moveKeep)
                 (st{redKings = remove (mid_jump start next) (redKings st)
                 ,redPieces = remove(mid_jump start next) (redPieces st)
                 ,blackKings = next:(remove start (blackKings st))})

              | status st == RedPlayer && (start)`elem`(redPieces st) && (snd (next) == 0) -- check if piece reached y=7
                 = make_jump_move (b:(rest))(moveKeep)
                 (st{blackKings = remove(mid_jump (start)(next))(blackKings st)
                 , blackPieces = remove(mid_jump (start)(next))(blackPieces st)
                 , redPieces = remove(start)(redPieces st)
                 , redKings = [next]++(redKings st)})

              | status st == BlackPlayer && (start)`elem`(blackPieces st) && (snd (next) == 7)
                 = make_jump_move (b:rest) (moveKeep)
                 (st{redKings = remove(mid_jump (start) (next))(redKings st)
                 , redPieces = remove(mid_jump (start) (next))(redPieces st)
                 , blackPieces = remove(start)(blackPieces st)
                 , blackKings = [next]++(blackKings st)})
                
              | status st == RedPlayer && start `elem` (redPieces st)
                 = make_jump_move (b:(rest))(moveKeep)
                 (st{blackKings = remove (mid_jump (start) (next))(blackKings st)
                 , blackPieces = remove (mid_jump (start) (next))(blackPieces st)
                 , redPieces = next:(remove start (redPieces st))})
                
              | status st == BlackPlayer && start `elem` (blackPieces st)
                 = make_jump_move (b:rest)(moveKeep)
                 (st{redKings = remove (mid_jump (start) (next)) (redKings st)
                 ,redPieces = remove(mid_jump (start) (next)) (redPieces st)
                 ,blackPieces = next:(remove start (blackPieces st))})
                 where
                   start = porky a 
                   next = porky b

replace start end [] = []
replace start end (a:as)
  | a == start = end:as
  | otherwise = a:(replace start end as)

remove :: Coord -> [Coord] -> [Coord] --remove component
remove x y = filter (/=x) y

mid_jump::Coord->Coord->Coord
mid_jump (x,y) (a,b) = (((x+a)`div` 2),((y+b)`div`2))

porky :: PorK a -> a
porky (P a) = a
porky (K a) = a

gameStats ::  GameState -> GameState
gameStats st
        | status st == RedPlayer = st{message = "Black Wins!", status = GameOver}
        | otherwise = st{message = "Red Wins!", status = GameOver}