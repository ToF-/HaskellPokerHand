module Ranking
where
import Data.List (sortBy, subsequences, groupBy)
import Data.Ord (comparing)
import Card

type Hand = [Card]

data Ranking = Fold
          | HighCard [Rank]
          | Pair [Rank]
          | ThreeOfAKind [Rank]
          | Flush [Rank]
    deriving (Eq, Ord, Show)

bestRanking :: [Card] -> Ranking
bestRanking cs | length cs < 7 = Fold
            | otherwise     = best cs
    where 
        best = maximum . 
               map groupAndRank .
               allHands .
               sortBy (flip (comparing rank))
        
        allHands :: [Card] -> [Hand]
        allHands = filter ((==5).length) . subsequences 

        ranks :: Hand-> [Rank] 
        ranks = map rank

        groupAndRank :: Hand -> Ranking
        groupAndRank h = ranking $ groups h
            where   

            ranks = map rank h
     
            ranking :: [Hand] -> Ranking
            ranking [[a],[b],[c],[d],[e]]  | isFlush [a,b,c,d,e] = Flush ranks
                                       | otherwise  = HighCard ranks 
    
            ranking [[a,b],[c],[d],[e]]    = Pair $ map rank [a,b,c,d,e]
        
            ranking [[a,b,c],[d],[e]]      = ThreeOfAKind $ map rank [a,b,c,d,e]

            groups :: Hand -> [[Card]]
            groups = sortBy groupSort . groupBy (same rank) . sortBy (comparing rank)

            groupSort :: [Card] -> [Card] -> Ordering
            groupSort g h | length g < length h = GT 
                      | length g > length h = LT 
                      | otherwise           = flip (comparing (rank . head)) g h

        same :: Eq(b) => (a -> b) -> a -> a -> Bool
        same f x y = f x == f y 
 
        isFlush :: [Card] -> Bool
        isFlush (c:cs) = all (\x -> suit x == suit c) cs
