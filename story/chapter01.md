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

Here's our main function:

```
main :: IO ()
main = interact displayRound 
```

Of course, we have to define the `displayRound` function. The built in function: `interact` requires a function of type `String -> String`.


