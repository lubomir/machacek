module GameTree where

import Data.Map (Map)
import Data.Tuple (swap)
import qualified Data.Map as M
import Control.Arrow (second)

data Player = P1 | P2
    deriving (Eq, Show)

type Probability = Double
type Payoff = Int

data HistoryEvent = HE { rolled :: Int  -- ^Actual value on the dice
                       , said :: Int    -- ^What player said
                       }
    deriving (Show, Eq, Ord)
type History = [HistoryEvent]

-- | Player's view of a single history event. Either he was rolling the dice
-- and knows what he rolled and said, or he only knows what the other player
-- said. To indicate situation where the player did not say anything yet, we
-- use 0.
--
data HistoryViewEvent = Heard Int               -- ^Other players turn
                      | Performed HistoryEvent  -- ^This players turn
                      deriving (Show, Eq, Ord)

type HistoryView = [HistoryViewEvent]

data GameTree = Nature [(Probability, GameTree)]
              | Decide HistoryView Player [GameTree]
              | Leaf Payoff
              deriving (Eq, Show)

type InformationSets a = Map HistoryView a

otherPlayer :: Player -> Player
otherPlayer P1 = P2
otherPlayer P2 = P1

payoffFor :: Player -> Payoff
payoffFor P1 = 1
payoffFor P2 = -1

insert :: History   -- ^Current history
       -> Int       -- ^What was on the dice
       -> Int       -- ^What was said
       -> History
insert h t s = HE { rolled = t, said = s } : h

lastSaid :: History -> Int
lastSaid []    = 0
lastSaid (e:_) = said e

-- | Create a history view for given player.
--
historyView :: Player -> History -> HistoryView
historyView p = go $ (if p == P1 then id else swap) (Performed, Heard . said)
  where
    go _     []     = []
    go (f,g) (e:es) = f e : go (g,f) es

-- | For dice with `k` sides compute game tree.
--
mkTree :: Int -> GameTree
mkTree k = rollDice [] P1
  where
    probs :: [Probability]
    probs = repeat $ 1 / fromIntegral k

    rollDice :: History -> Player -> GameTree
    rollDice h p = if lastSaid h == k
                        then Leaf $ payoffFor $ otherPlayer p
                        else Nature $ zip probs $ map (sayNum h p) [1..k]

    sayNum :: History -> Player -> Int -> GameTree
    sayNum h p i = Decide view p $ map say [firstOption..k]
      where
        view = historyView p $ insert h i 0
        firstOption = max i (lastSaid h + 1)
        say n = decideCall (insert h i n) (otherPlayer p)

    decideCall :: History -> Player -> GameTree
    decideCall h p = Decide (historyView p h) p [rollDice h p, call h p]

    call :: History -> Player -> GameTree
    call (e:_) p
      | rolled e == said e = Leaf $ payoffFor $ otherPlayer p
      | otherwise          = Leaf $ payoffFor p
    call _ _ = error "This can never happen"


getSet :: Int -> HistoryView -> InformationSets Int -> (Int,InformationSets Int,Int)
getSet new hv set = case M.lookup hv set of
                        Nothing  -> (new, M.insert hv new set, new + 1)
                        Just id' -> (id', set, new)


indent :: (a, InformationSets b, Int) -> [String] -> [String]
indent (_,_,n) = (replicate n '\t' :)

indentWidth :: Int
indentWidth = 1

incIndent :: (a,InformationSets b,Int) -> (a,InformationSets b,Int)
incIndent (x,y,n) = (x,y,n+indentWidth)

decIndent :: (a,InformationSets b,Int) -> (a,InformationSets b,Int)
decIndent (x,y,n) = (x,y,n-indentWidth)

showTree :: GameTree -> String
showTree = concat . fst . go (1, M.empty, 0)
  where
    go :: (Int, InformationSets Int, Int) -> GameTree -> ([String], (Int, InformationSets Int, Int))
    go acc l@(Leaf _)       = (indent acc [show l, "\n"], acc)
    go acc (Nature ts)=
        second decIndent $ foldl walkChildren (indent acc ["Nature\n"], incIndent acc)
                         $ map snd ts
    go acc@(i,s,ind) (Decide hv p ts)
        = let (id_,newS,newI) = getSet i hv s
              str = indent acc ["Decide ",show id_," ",show p,"\n"]
          in second decIndent $ foldl walkChildren (str, incIndent (newI,newS,ind)) ts

    walkChildren :: ([String], (Int, InformationSets Int, Int))
                 -> GameTree
                 -> ([String], (Int, InformationSets Int, Int))
    walkChildren (have, acc) t = let (str, newAcc) = go acc t
                                 in (have ++ str, newAcc)
