Bob: So how are we going to solve this problem?

ToF: Well I see three questions that will require a bit of thinking..

Bob: What are those questions?

ToF: One is obviously: how do we compare well formed, 5 cards poker hands?

Bob: Yes, we have the rules for that. 

ToF: The second question is: how do we form different hands from each entry line so that we can extract the best possible hand for that player?

Bob: For lines with less than seven cards, that is not necessary. I guess we'll have to find subsequences or something like that. 

ToF: Yes.. The third question is the simplest: how do we display correctly the entry lines along with the scores?

Bob: Wow, solving all those problems is a lot of work. And the customer wants to see a running program very soon!

ToF: Well in that case, what if we showed him a program that works only for High cards for the first version?

Bob: Hey, solving simpler versions of the problem first, I like that! We still have the subsequencing problem, though..

ToF: Well if we recognize only High Cards hands, we just have to sort the cards and take the first five cards on each line. 

Bob: Right!

ToF: Let's go then!

Bob: Wait! Should'nt we start by defining some tests for our first version of the program?

ToF: You are absolutely right. Let's see: what do we have as test cases for that first version?

Bob: Let's list them:

- No winner: all players fold. This is a stupid case maybe, but that could make our program fail.
- One winner: some players fold, the other have High Card hands, one of them have the highest ranking card as first card
- Two winners: two players are in a tie, with same High Card hand
  
ToF: Okay. We need to prepare two things: a test harness, and a main program. The main program will read its input, do some computations, and print the result. 

ToF: Here's our main function:

```
import PokerHand

main :: IO ()
main = interact displayRound 
```

ToF: Of course, we have to define the `displayRound` function, in the module called `PokerHand`. Since it is given to `interact` it should be a function of type `String -> String`.

Bob: Here's the data for the first acceptance test case:

```
Ac Qc Ks Kd 9d 3c
9h 5s
```

ToF: Okay, let's write a test program. We will use `hspec` as it makes test execution really clear about what is specified.
```
import Test.Hspec
import PokerHand

main :: IO ()
main = hspec $ do
    describe "displayRound function" $ do
        it "should show no winner on a round with only players who fold" $ do
            displayRound "Ac Qc Ks Kd 9d 3c\n9h 5s\n" `shouldBe` "Ac Qc Ks Kd 9d 3c\n9h 5s\n"
```

Bob: Let's now write the `PokerHand` module. It's really easy:

```
module PokerHand
where

displayRound :: String -> String
displayRound = id
```
Since in that case, the entry is just repeated without any new information.

ToF: Well, even if that program does not really compute anything, we could still enhance its structure. For example, it would be better to express what is displayed as score lines, those being the result of computations done on entry lines:

```
module PokerHand
where

type EntryLine = String
type ScoreLine = String

displayRound :: [EntryLine] -> [ScoreLine]
displayRound = id
```

Bob: In that case we have to change the test:
```
import Test.Hspec
import PokerHand

main :: IO ()
main = hspec $ do
    describe "displayRound function" $ do
        it "should show no winner on a round with only players who fold" $ do
            displayRound ["Ac Qc Ks Kd 9d 3c"
                         ,"9h 5s"            ] 
              `shouldBe` ["Ac Qc Ks Kd 9d 3c"
                         ,"9h 5s"            ]
```

ToF: We also have to change our main program, so that the contents of the interaction is split in lines for our function, and the result put back into the form of a block of text:

```
module Main
where
import PokerHand

main :: IO ()
main = interact (unlines . displayRound . lines)
```

ToF: Okay! We need to move forward a bit. What would be a simple test that we could write that would require us to really compute a score line instead of just replicating the input?

Bob: The case where we have a winner. In that case, that info is mentioned, along with the "winner" tag:

```
        it "should show a winner on a round with only one player with a High Card" $ do
            displayRound ["Ac Qc Ks Kd 9d 3c"
                         ,"9h 5s"
                         ,"4d 2s Ks Kd 9d 3c 6d"]
              `shouldBe` ["Ac Qc Ks Kd 9d 3c"
                         ,"9h 5s"
                         ,"4d 2s Ks Kd 9d 3c 6d High Card (winner)"]
```

ToF: So now, the value of the score line depends on the entry. It's still very simple: if the entry contains less than 7 cards, the player folded, we just replicate the line. If the entry contains 7 cards, we assume this is a High Card, and this line is the winner: 
    
```
displayRound :: [EntryLine] -> [ScoreLine]
displayRound = map score

score :: EntryLine -> ScoreLine
score line | length (words line) == 7 = line ++ " High Card (winner)" 
           | otherwise                = line
```

Bob: That passes the test! But that's not very clear. Let's create a helper for counting cards:

```
score :: EntryLine -> ScoreLine
score line | size line == 7 = line ++ " High Card (winner)" 
           | otherwise      = line

size :: EntryLine -> Int
size = length . words 
```

ToF: Of course that is still very unsatifying. What happens if there are several lines containing 7 cards?

Bob: Then, the winner is on the line with the best High Card ranking; here's a test:

```
        it "should show a winner on a round with several High Card, with one ranking higher" $ do
            displayRound ["9h 5s"
                         ,"4d 2s Qs Kd 9d 3c 6d" 
                         ,"4d 2s Ks Kd 9d 3c 6d"]
              `shouldBe` ["9h 5s"
                         ,"4d 2s Qs Kd 9d 3c 6d High Card"
                         ,"4d 2s Ks Kd 9d 3c 6d High Card (winner)"]
```

ToF: Wow, now the computation for score is much more complex:
- if the line doesn't contain 7 cards, just output the line
- if the line does contain 7 cards, mention "High Card" at the end of the line
- if the line does contain 7 cards, and the high card hand given by the 5 biggest cards of that line has the best rank in all the round, add the winner tag to that line.

Bob: I propose that we remove that last test for now, and proceed with smaller steps.

ToF: Or we can make the test past with fake answers and try to refactor. 

Bob: Okay. Let's try.

```
score :: EntryLine -> ScoreLine
score line | line == "4d 2s Ks Kd 9d 3c 6d" = line ++ " High Card (winner)" 

score line | size line == 7 = line ++ " High Card" 
           | otherwise      = line
```
ToF: Eeeeww..

