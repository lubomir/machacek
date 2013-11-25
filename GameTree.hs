module GameTree where

import Data.List (foldl')
import Data.Map (Map)
import Data.Tuple (swap)
import Data.Maybe
import qualified Data.Map as M
import Control.Arrow (second)

data Player = P1 | P2
    deriving (Eq, Ord, Show)

type Probability = Double
type Payoff = Double
type Act = Int

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
historyView p = go ((if p == P1 then id else swap) (Performed, Heard . said)) . reverse
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

data LoopAcc = LA { pool     :: [Act]
                  , assigned :: InformationSets [Act]
                  , matrix   :: [(Sequence, Sequence, Double)]
                  }

getActs :: LoopAcc -> Int -> HistoryView -> ([Act], LoopAcc)
getActs la n hv = case M.lookup hv (assigned la) of
                    Nothing   -> let acts = take n $ pool la
                                 in (acts, la { assigned = M.insert hv acts (assigned la)
                                              , pool = drop n $ pool la
                                              })
                    Just acts -> (acts, la)

type Sequence = [Act]
type SeqPair = M.Map Player Sequence

addAct :: Player -> Act -> SeqPair -> SeqPair
addAct p a = M.insertWith (++) p [a]

getSequence :: Player -> SeqPair -> Sequence
getSequence p = fromMaybe [] . M.lookup p

mkMatrix :: GameTree -> (InformationSets [Act], [(Sequence, Sequence, Double)])
mkMatrix tree = let res = go 1 M.empty (LA [1..] M.empty []) tree
                in (assigned res, matrix res)
  where
    go :: Double -> SeqPair -> LoopAcc -> GameTree -> LoopAcc
    go p sp acc (Nature ts) = foldl' natHelper acc ts
      where
        natHelper :: LoopAcc -> (Probability, GameTree) -> LoopAcc
        natHelper acc' (p',t) = go (p * p') sp acc' t

    go p sp acc (Leaf x) = let val = (getSequence P1 sp, getSequence P2 sp, p * x)
                           in acc { matrix = val : matrix acc }

    go p sp acc'' (Decide hv pl ts) = foldl' decHelper acc' $ zip acts ts
      where
        (acts, acc') = getActs acc'' (length ts) hv
        decHelper :: LoopAcc -> (Act, GameTree) -> LoopAcc
        decHelper acc (a,t) = go p (addAct pl a sp) acc t


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
    go :: (Int, InformationSets Int, Int)
       -> GameTree
       -> ([String], (Int, InformationSets Int, Int))
    go acc l@(Leaf _)  = (indent acc [show l, "\n"], acc)
    go acc (Nature ts) =
        second decIndent $ foldl' walkChildren (indent acc ["Nature\n"], incIndent acc)
                         $ map snd ts
    go acc@(i,s,ind) (Decide hv p ts)
        = let (id_,newS,newI) = getSet i hv s
              str = indent acc ["Decide ",show id_," ",show p,"\n"]
          in second decIndent $ foldl' walkChildren (str, incIndent (newI,newS,ind)) ts

    walkChildren :: ([String], (Int, InformationSets Int, Int))
                 -> GameTree
                 -> ([String], (Int, InformationSets Int, Int))
    walkChildren (have, acc) t = let (str, newAcc) = go acc t
                                 in (have ++ str, newAcc)
