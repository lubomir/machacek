module GameTree where

data Player = P1 | P2
    deriving (Eq, Show)

type Probability = Double
type Payoff = Int

data GameTree = Nature [(Probability, GameTree)]
              | Decide Player [GameTree]
              | Leaf Payoff
              deriving (Eq, Show)

data HistoryEvent = HE { thrown :: Int, said :: Int }
    deriving (Show)
type History = [HistoryEvent]

otherPlayer :: Player -> Player
otherPlayer P1 = P2
otherPlayer P2 = P1

payoffFor :: Player -> Payoff
payoffFor P1 = 1
payoffFor P2 = -1

insert :: History -> Int -> Int -> History
insert h t s = HE { thrown = t, said = s } : h

lastSaid :: History -> Int
lastSaid []    = 0
lastSaid (e:_) = said e

-- | For dice with `k` sides compute game tree.
--
mkTree :: Int -> GameTree
mkTree k = throwDice [] P1
  where
    probs :: [Probability]
    probs = repeat $ 1 / fromIntegral k

    throwDice :: History -> Player -> GameTree
    throwDice h p = if lastSaid h == k
                        then Leaf $ payoffFor $ otherPlayer p
                        else Nature $ zip probs $ map (sayNum h p) [1..k]

    sayNum :: History -> Player -> Int -> GameTree
    sayNum h p i = Decide p $ map say [firstOption..k]
      where
        firstOption = max i (lastSaid h + 1)
        say n = decideCall (insert h i n) (otherPlayer p)

    decideCall :: History -> Player -> GameTree
    decideCall h p = Decide p [throwDice h p, call h p]

    call :: History -> Player -> GameTree
    call (e:_) p
      | thrown e == said e = Leaf $ payoffFor $ otherPlayer p
      | otherwise          = Leaf $ payoffFor p
    call _ _ = error "This can never happen"


-- TEST DATA

treeForTwo :: GameTree
treeForTwo = Nature [ (0.5,Decide P1 [ Decide P2 [ Nature [ (0.5,Decide P2 [ Decide P1 [ Leaf (-1) , Leaf 1 ] ]) , (0.5,Decide P2 [ Decide P1 [ Leaf (-1) , Leaf (-1) ] ]) ] , Leaf 1 ] , Decide P2 [ Leaf 1 , Leaf (-1) ] ]) , (0.5,Decide P1 [ Decide P2 [ Leaf 1 , Leaf 1 ] ]) ]
