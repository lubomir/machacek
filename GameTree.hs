module GameTree where

import           Data.List  (foldl', nub, sort, tails)
import           Data.Map   (Map)
import qualified Data.Map   as M
import           Data.Maybe (fromMaybe, fromJust)
import           Data.Tuple (swap)
import Data.Array
import Control.Arrow (first, second)

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

data LoopAcc = LA { pool     :: ([Act], [Act])
                  , assigned :: InformationSets (Sequence, [Act])
                  , matrix   :: [(Sequence, Sequence, Double)]
                  }

getActsFor :: Int                       -- ^How many actions we want
           -> Player                    -- ^Whose actions we want
           -> ([Act], [Act])            -- ^Pool of actions
           -> ([Act], ([Act], [Act]))   -- ^(wanted actions, new pool)
getActsFor n P1 = (\((a,b),c) -> (a,(b,c))) . first  (splitAt n)
getActsFor n P2 = (\(a,(b,c)) -> (b,(a,c))) . second (splitAt n)

-- | Get actions for given player and his view of history. If the actions do
-- not exist yet, draw new actions from pool and assign them to information
-- set given by this history view.
--
getActs :: Player           -- ^Whose actions we want
        -> Sequence         -- ^Action sequence used to reach this information set
        -> LoopAcc          -- ^Current loop state
        -> Int              -- ^How many actions we want
        -> HistoryView      -- ^Where to store the actions
        -> ([Act], LoopAcc) -- ^(wanted actions, neww loop state)
getActs p s la n hv =
    case M.lookup hv (assigned la) of
        Nothing   -> let (acts, pool') = getActsFor n p $ pool la
                     in (acts, la { assigned = M.insert hv (s,acts) (assigned la)
                                  , pool = pool'
                                  })
        Just (_,acts) -> (acts, la)

type Sequence = [Act]
type SeqPair = M.Map Player Sequence

viewBelongsTo :: HistoryView -> Player -> Bool
(Performed _ : _) `viewBelongsTo` P1 = True
(Heard _     : _) `viewBelongsTo` P2 = True
_                 `viewBelongsTo` _  = False

addAct :: Player -> Act -> SeqPair -> SeqPair
addAct p a = M.insertWith (++) p [a]

getSequence :: Player -> SeqPair -> Sequence
getSequence p = fromMaybe [] . M.lookup p

mkActions :: GameTree -> (InformationSets (Sequence, [Act]), [(Sequence, Sequence, Double)])
mkActions tree = let res = go 1 M.empty (LA ([1..], [1..]) M.empty []) tree
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
        (acts, acc') = getActs pl (getSequence pl sp) acc'' (length ts) hv
        decHelper :: LoopAcc -> (Act, GameTree) -> LoopAcc
        decHelper acc (a,t) = go p (addAct pl a sp) acc t


getSequenceMap :: [(Sequence, Sequence, Double)] -> ([(Sequence,Int)],[(Sequence,Int)])
getSequenceMap ts = let (xs', ys', _) = unzip3 ts
                        xMap = flip zip [0..] $ sort $ nub $ sort $ concatMap tails $ xs'
                        yMap = flip zip [0..] $ sort $ nub $ sort $ concatMap tails $ ys'
                    in (xMap, yMap)


mkPayoffMatrix :: [(Sequence, Sequence, Double)] -> Array (Int, Int) Double
mkPayoffMatrix ts = let (xMap, yMap) = getSequenceMap ts
                        nullArray = listArray ((0,0),(length xMap,length yMap)) $ repeat 0
                    in foldl' upd nullArray $ map (\(x,y,p) -> ((ml x xMap,ml y yMap), p)) ts
  where
    ml = (fromJust.) . lookup
    upd :: Array (Int,Int) Double -> ((Int,Int),Double) -> Array (Int,Int) Double
    upd arr (i,p) = arr // [(i,p+arr ! i)]

mkConstraintMatrix :: Player                            -- ^Player we are interested in
                   -> [(Sequence,Int)]                  -- ^This players' actions mapping
                   -> InformationSets (Sequence, [Act]) -- ^All info sets
                   -> [[Double]]
mkConstraintMatrix p m is = (1:replicate (len-1) 0) : map toEq sets
  where
    sets = filter ((`viewBelongsTo` p) . fst) $ M.toList is
    len = 1 + length (nub $ concatMap (snd . snd) sets)
    toEq (_k,(s,as)) = mkRow (ml s) (map (ml . (:s)) as)
    ml s = fromJust $ lookup s m

    mkRow :: Int -> [Int] -> [Double]
    mkRow x vs = reverse (go [] 0 vs)
      where
        go acc n (y:ys)
            | n >= len  = error "This should never happen"
            | n == x    = go (-1:acc) (n+1) (y:ys)
            | n == y    = go (1:acc)  (n+1) ys
            | otherwise = go (0:acc)  (n+1) (y:ys)
        go acc n []
            | n >= len  = acc
            | n == x    = go (-1:acc) (n+1) []
            | otherwise = go (0:acc)  (n+1) []
