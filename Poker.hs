-- Authors:
--  Michael Pereira (500 896 409)
--  Justin Miraflor (500 908 894)

module Poker where
    import Data.Char
    import Data.List as L
    import Data.Ord
    import Data.Set as S 

    deal :: [Int] -> [String]
    deal cards = do

        --Deal cards
        let hand1 = everyOther cards
        let hand2 = cards L.\\ hand1

        --Get suits
        let h1 = L.map getCard hand1
        let h2 = L.map getCard hand2

        --Sort cards
        let hand1Final = sortBy (comparing fst) h1
        let hand2Final = sortBy (comparing fst) h2

        --Determine the ranking of each hand
        let hand1Rank = determineHandRank hand1Final
        let hand2Rank = determineHandRank hand2Final

        --Decide which hand is the winner
        if(hand1Rank < hand2Rank)then
            printWinner(hand1Final)
        else if(hand2Rank < hand1Rank)then
            printWinner(hand2Final)
        else
            printWinner(determineTieBreaker hand1Final  hand2Final hand1Rank)

    -- Prints Winner
    printWinner :: [(Int, String)] -> [String]
    printWinner hand = do
        let winnerUnsorted = L.map getCardR hand
        let winnerCorrectV = sort winnerUnsorted
        let winnerFinal = L.map getString winnerCorrectV
        winnerFinal

    -- Gets Correct Card Number
    getCardR :: (Int, String) -> (Int, String)
    getCardR tuple = do
        let suit = snd tuple
        let card = fst tuple
        let cardNum = case (card) of 
                        14 -> 1
                        _ -> card
        (cardNum, suit)

    -- Converts to String Format
    getString :: (Int, String) -> String
    getString tuple = do
        let suit = snd tuple
        let card = fst tuple
        (show(card)++suit)

    --------------Tie Breaking---------------
    -- Goes to Appropriate Tie Breaking Operation
    determineTieBreaker :: [(Int, String)] -> [(Int, String)] -> Int -> [(Int, String)]
    determineTieBreaker hand1 hand2 rank = do
        case(rank) of 
            1 -> tbRoyalFlush hand1 hand2
            2 -> tbMixedOperations1 hand1 hand2
            3 -> tbMixedOperations2 hand1 hand2
            4 -> tbMixedOperations2 hand1 hand2
            5 -> tbMixedOperations1 hand1 hand2
            6 -> tbMixedOperations1 hand1 hand2
            7 -> tbMixedOperations2 hand1 hand2
            8 -> tbMixedOperations1 hand1 hand2
            9 -> tbPair hand1 hand2
            10 -> tbMixedOperations1 hand1 hand2

    -- Tie-breaker for Royal Flush
    tbRoyalFlush :: [(Int, String)] -> [(Int, String)] -> [(Int, String)]
    tbRoyalFlush hand1 hand2 = do
        compareSuits hand1 hand2

    -- Tie-breaker for Straight Flush, Flush, Straight, and High Card
    tbMixedOperations1 :: [(Int, String)] -> [(Int, String)] -> [(Int, String)]
    tbMixedOperations1 hand1 hand2 = do
        let winner = compareRanks hand1 hand2
        
        if(winner==[(0,"Equal")])then
            compareSuits hand1 hand2
        else
            winner
    
    -- Tie-breaker for Four of a Kind, Full House, and Three Of A Kind
    tbMixedOperations2 :: [(Int, String)] -> [(Int, String)] -> [(Int, String)]
    tbMixedOperations2 hand1 hand2 = do
        let cards1 = fst (unzip hand1)
        let cards2 = fst (unzip hand2) 

        let n1 = cards1 !! 2
        let n2 = cards2 !! 2

        if(n1>n2)then
            hand1
        else
            hand2

    -- Tie-breaker for Pair
    tbPair :: [(Int, String)] -> [(Int, String)] -> [(Int, String)]
    tbPair hand1 hand2 = do
        let cards1 = fst (unzip hand1)
        let cards2 = fst (unzip hand2)
        let c11 = cards1 !! 1 --2nd card for hand1
        let c12 = cards1 !! 3 --3rd card for hand1
        let c21 = cards2 !! 1 --2nd card for hand2
        let c22 = cards2 !! 3 --3rd card for hand2

        if((tbPairCheck hand1 c11 c12)>(tbPairCheck hand2 c21 c22))then
            hand1
        else if((tbPairCheck hand1 c11 c12)<(tbPairCheck hand2 c21 c22))then
            hand2
        else
            if(compareRanks hand1 hand2 == [(0,"Equal")])then
                compareSuits hand1 hand2
            else
                compareRanks hand1 hand2

    tbPairCheck :: [(Int, String)] -> Int -> Int -> Int
    tbPairCheck hand n1 n2 = do
        let cards = fst (unzip hand)
        if(countElem n1 cards == 2)then
            n1
        else
            n2      

    compareRanks :: [(Int, String)] -> [(Int, String)] -> [(Int, String)]
    compareRanks hand1 hand2 = do
        let cards1 = fst (unzip hand1)
        let cards2 = fst (unzip hand2)

        let total1 = L.foldr (+) 0(cards1)
        let total2 = L.foldr (+) 0(cards2)
        
        if(total1>total2)then
            hand1
        else if(total2>total1)then
            hand2
        else
            [(0,"Equal")]

    -- Determines which hand is higher based on suits
    compareSuits :: [(Int, String)] -> [(Int, String)] -> [(Int, String)] 
    compareSuits hand1 hand2 = do
        let suit1 = snd (unzip hand1)
        let suit2 = snd (unzip hand2)

        let total1 = L.foldr (+) 0(L.map (\c -> (ord c)) (concat suit1))
        let total2 = L.foldr (+) 0(L.map (\c -> (ord c)) (concat suit2))
        
        if(total1>total2)then
            hand1
        else
            hand2

    --------------CHECK RANKING--------------
    -- Check Royal Flush
    checkRoyalFlush :: [(Int, String)] -> Bool
    checkRoyalFlush hand = do
        let cards = fst (unzip hand)
        
        let straight = do
            case (cards) of
                [10,11,12,13,14] -> True
                _ -> False

        let suits = checkSuit hand

        straight == True && suits == True

    -- Check Straight Flush
    checkStraightFlush :: [(Int, String)] -> Bool
    checkStraightFlush hand = do
        checkStraight hand == True && checkSuit hand == True  

    -- Check Full House
    checkFullHouse :: [(Int, String)] -> Bool
    checkFullHouse hand = do
        let cards = fst (unzip hand)
        let hd = head cards
        let tl = last cards

        if((countElem hd cards == 3) && (countElem tl cards == 2)) then
            True
        else if((countElem hd cards == 2) && (countElem tl cards == 3)) then
            True
        else
            False 
    
    -- Count Occurences of Element in List
    countElem :: Eq a => a -> [a] -> Int
    countElem x = length . L.filter (== x)

    -- Check Flush
    checkFlush :: [(Int, String)] -> Bool
    checkFlush hand = do
        checkSuit hand == True && checkStraight hand == False

    -- Check X Of A Kind
    checkXOfAKind :: [(Int, String)] -> Int -> Bool
    checkXOfAKind hand n = do
        let cards = fst (unzip hand)     
        let set = S.fromList cards

        if(length set == 4 && n == 2) then
            if((countElem (cards !! 1) cards) == 2 || (countElem (cards !! 3) cards) == 2) then
                True
            else
                False
        else if(length set > 1) then
            if(countElem (cards !! 2) cards == n) then
                True
            else
                False
        else
            False

    -- Check Straight
    checkStraight :: [(Int, String)] -> Bool
    checkStraight hand = do
        let cards = fst(unzip hand)
        let hd = head cards

        let fullSuit = [hd,hd+1,hd+2,hd+3,hd+4]

        if (cards == fullSuit || cards == [2,3,4,5,14]) then 
            True
        else
            False

    -- Check Two Pair
    checkTwoPair :: [(Int, String)] -> Bool
    checkTwoPair hand = do
        let cards = fst (unzip hand)

        let count1 = countElem (cards !! 1) cards
        let count2 = countElem (cards !! 3) cards

        if (count1 == 2 && count2 == 2) then
            True   
        else
            False

    --------------Misc. Methods--------------
    -- Gets the suit of the card
    getCard :: Int -> (Int, String)
    getCard x = do
        let suit = getSuit x
        let card = case (rem x 13) of 
                       0 -> 13
                       1 -> 14
                       _ -> (rem x 13)
        case (suit) of 0 -> (card, "C")
                       1 -> (card, "D")
                       2 -> (card, "H")
                       3 -> (card, "S")
                       _ -> (0, "ArgumentError")
    
    getSuit :: Int -> Int
    getSuit x = do
        if (rem x 13 == 0)then
            ((div x 13)-1)
        else
            (div x 13)

    -- Determine what ranking the hand has
    determineHandRank :: [(Int,String)] -> Int
    determineHandRank hand
        |checkRoyalFlush hand = 1
        |checkStraightFlush hand = 2
        |checkXOfAKind hand 4 = 3
        |checkFullHouse hand = 4
        |checkFlush hand = 5
        |checkStraight hand = 6
        |checkXOfAKind hand 3 = 7
        |checkTwoPair hand = 8
        |checkXOfAKind hand 2 = 9
        |True = 10

    -- Check if all suits in hand are the same
    checkSuit :: [(Int, String)] -> Bool
    checkSuit hand = do
        let suits = snd (unzip hand)
        let set = S.fromList suits

        if(length set == 1) then
            True
        else
            False
    
    -- Every other (Used to deal cards)
    everyOther :: [Int] -> [Int]
    everyOther = L.map snd . L.filter (odd . fst) . zip [0..]  
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        