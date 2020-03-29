toDigitsRev :: Integer -> [Integer]
toDigitsRev x
        | x <= 0
        = []
        | otherwise
        = let d = x `div` 10
              m = x `mod` 10
          in  m : toDigitsRev d

toDigits = reverse . toDigitsRev

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev []           = []
doubleEveryOtherRev [x         ] = [x]
doubleEveryOtherRev (x : y : ys) = [x, 2 * y] ++ doubleEveryOtherRev ys

doubleEveryOther = reverse . doubleEveryOtherRev . reverse

sumDigits :: [Integer] -> Integer
sumDigits = foldr (\x acc -> acc + (sum . toDigits $ x)) 0

validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits $ x) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)

data State = State { source :: Peg, target :: Peg, aux :: Peg, moves :: [Move]}

move :: Integer -> State -> State
move 0 s = s
move n (State source target aux moves) =
        let (State _ _ _ preMoves) =
                            move (n - 1) (State source aux target moves)
            newMoves = (source, target) : preMoves
        in  move (n - 1) (State aux target source newMoves)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n p1 p2 p3 = reverse . moves $ move n (State p1 p2 p3 [])

