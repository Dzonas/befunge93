{-# LANGUAGE RecordWildCards #-}

module Befunge93 where

import Data.Char
import System.Random (StdGen, uniformR)

type Stack = [Int]

type Output = String

type InstrPtr = (Int, Int)

type Instruction = Char

type Width = Int

type Height = Int

data Direction = L | R | U | D | X deriving (Show)

data Mode = StringMode | NormalMode deriving (Show)

randomizeDirection :: StdGen -> (Direction, StdGen)
randomizeDirection gen
  | n == 1 = (L, gen')
  | n == 2 = (R, gen')
  | n == 3 = (U, gen')
  | n == 4 = (D, gen')
  | otherwise = error "Impossible"
  where
    (n, gen') = uniformR (1 :: Int, 4) gen

data State = State {stack :: [Int], instrPtr :: (Int, Int), direction :: Direction, width :: Int, height :: Int, output :: String, gen :: StdGen, mode :: Mode, trampoline :: Bool, instructions :: [[Instruction]]} deriving (Show)

interpret :: StdGen -> String -> String
interpret gen commands = reverse (interpret' initialState)
  where
    initialState = State [] (0, 0) R width height "" gen NormalMode False normalizedCommands
    height = length normalizedCommands
    width = length $ head normalizedCommands
    normalizedCommands = normalize commands

interpret' :: State -> String
interpret' s@State {..} = case nextFun of
  Just f -> interpret' (move (f s))
  Nothing -> output
  where
    nextFun = mapCharToFun nextInstr mode
    nextInstr = instructions !! y !! x
    (y, x) = instrPtr

normalize :: String -> [[Char]]
normalize ss = map (rpad toN) lns
  where
    toN = maximum (map length lns)
    lns = lines ss

rpad :: Int -> String -> String
rpad n ss = ss ++ pad
  where
    pad = replicate (n - length ss) ' '

mapCharToFun :: Char -> Mode -> Maybe (State -> State)
mapCharToFun c StringMode
  | c /= '"' = Just $ pushString c
  | otherwise = Just toggleStringMode
mapCharToFun '+' _ = Just add
mapCharToFun '-' _ = Just sub
mapCharToFun '*' _ = Just mul
mapCharToFun '/' _ = Just div'
mapCharToFun '%' _ = Just mod'
mapCharToFun '!' _ = Just not'
mapCharToFun '`' _ = Just greaterThan
mapCharToFun '>' _ = Just startMovingRight
mapCharToFun '<' _ = Just startMovingLeft
mapCharToFun '^' _ = Just startMovingUp
mapCharToFun 'v' _ = Just startMovingDown
mapCharToFun '?' _ = Just startMovingRandomly
mapCharToFun '_' _ = Just popHorizontal
mapCharToFun '|' _ = Just popVertical
mapCharToFun '"' _ = Just toggleStringMode
mapCharToFun ':' _ = Just duplicateStack
mapCharToFun '\\' _ = Just swapStack
mapCharToFun '$' _ = Just popAndDiscard
mapCharToFun '.' _ = Just outputInt
mapCharToFun ',' _ = Just outputChar
mapCharToFun '#' _ = Just enableTrampoline
mapCharToFun 'p' _ = Just put
mapCharToFun 'g' _ = Just get
mapCharToFun ' ' _ = Just noOp
mapCharToFun '@' _ = Nothing
mapCharToFun n _
  | isDigit n = Just $ pushDigitToStack n
  | otherwise = error "dupa"

pop :: [a] -> (a, [a])
pop (x : xs) = (x, xs)
pop _ = error "trying to pop from empty stack"

pop2 :: [a] -> (a, a, [a])
pop2 (x : y : xs) = (x, y, xs)
pop2 _ = error "trying to pop 2 elements from stack that has less than 2 elements"

pop3 :: [a] -> (a, a, a, [a])
pop3 (x : y : z : xs) = (y, x, z, xs)
pop3 _ = error "trying to pop 3 elements from stack that has less than 3 elements"

add :: State -> State
add State {..} = State {stack = a + b : stack', ..}
  where
    (a, b, stack') = pop2 stack

sub :: State -> State
sub State {..} = State {stack = stack'', ..}
  where
    stack'' = b - a : stack'
    (a, b, stack') = pop2 stack

mul :: State -> State
mul State {..} = State {stack = stack'', ..}
  where
    stack'' = a * b : stack'
    (a, b, stack') = pop2 stack

div' :: State -> State
div' State {..} = State {stack = stack''', ..}
  where
    stack''' = (if a == 0 then 0 else b `div` a) : drop 2 stack''
    (b, stack'') = pop stack'
    (a, stack') = pop stack

mod' :: State -> State
mod' State {..} = State {stack = stack''', ..}
  where
    stack''' = (if a == 0 then 0 else b `mod` a) : stack''
    (b, stack'') = pop stack'
    (a, stack') = pop stack

not' :: State -> State
not' State {..} = State {stack = stack'', ..}
  where
    stack'' = y : stack'
    y = if x == 0 then 1 else 0
    (x, stack') = pop stack

greaterThan :: State -> State
greaterThan State {..} = State {stack = c : stack'', ..}
  where
    c = if b > a then 1 else 0
    (b, stack'') = pop stack'
    (a, stack') = pop stack

startMovingRight :: State -> State
startMovingRight State {..} = State {direction = R, ..}

startMovingLeft :: State -> State
startMovingLeft State {..} = State {direction = L, ..}

startMovingUp :: State -> State
startMovingUp State {..} = State {direction = U, ..}

startMovingDown :: State -> State
startMovingDown State {..} = State {direction = D, ..}

startMovingRandomly :: State -> State
startMovingRandomly State {..} = State {direction = X, ..}

popHorizontal :: State -> State
popHorizontal State {..} = State {stack = stack', direction = direction', ..}
  where
    direction' = if n == 0 then R else L
    (n, stack') = pop stack

popVertical :: State -> State
popVertical State {..} = State {stack = stack', direction = direction', ..}
  where
    direction' = if n == 0 then D else U
    (n, stack') = pop stack

toggleStringMode :: State -> State
toggleStringMode State {mode = StringMode, ..} = State {mode = NormalMode, ..}
toggleStringMode State {mode = NormalMode, ..} = State {mode = StringMode, ..}

duplicateStack :: State -> State
duplicateStack State {stack, ..} = State {stack = stack', ..}
  where
    stack' = duplicateStack' stack

duplicateStack' :: [Int] -> [Int]
duplicateStack' [] = [0]
duplicateStack' (x : xs) = x : x : xs

swapStack :: State -> State
swapStack State {stack, ..} = State {stack = stack', ..}
  where
    stack' = swapStack' stack

swapStack' :: [Int] -> [Int]
swapStack' [] = error "trying to swap empty stack"
swapStack' [x] = [0, x]
swapStack' (x : y : xs) = y : x : xs

popAndDiscard :: State -> State
popAndDiscard State {stack, ..} = State {stack = stack', ..}
  where
    (_, stack') = pop stack

popAndDiscard' :: [Int] -> [Int]
popAndDiscard' [] = error "trying to pop and discard from empty stack"
popAndDiscard' xs = tail xs

pushString :: Char -> State -> State
pushString c State {stack, ..} = State {stack = stack', ..}
  where
    stack' = ord c : stack

noOp :: State -> State
noOp = id

outputInt :: State -> State
outputInt State {..} = State {stack = newStack, output = newOutput, ..}
  where
    newStack = tail stack
    newOutput = reverse (show (head stack)) ++ output

outputChar :: State -> State
outputChar State {..} = State {stack = stack', output = output', ..}
  where
    output' = chr n : output
    (n, stack') = pop stack

enableTrampoline :: State -> State
enableTrampoline State {..} = State {trampoline = True, ..}

pushDigitToStack :: Char -> State -> State
pushDigitToStack digit State {..} = State {stack = newStack, ..}
  where
    newStack = digitToInt digit : stack

put :: State -> State
put State {instructions, stack, ..} = State {instructions = instructions', stack = stack', ..}
  where
    instructions' = put' y x (chr v) instructions
    (y, x, v, stack') = pop3 stack

put' :: Int -> Int -> Char -> [[Char]] -> [[Char]]
put' y x v = replace y (replace x (const v))

get :: State -> State
get State {instructions, stack, instrPtr, ..} = State {stack = stack'', ..}
  where
    stack'' = ord v : stack'
    v = instructions !! y !! x
    (y, x, stack') = pop2 stack

replace :: Int -> (a -> a) -> [a] -> [a]
replace _ _ [] = []
replace i f (x : xs)
  | i == 0 = f x : xs
  | otherwise = x : replace (i - 1) f xs

move :: State -> State
move State {direction = R, trampoline, ..} = State {instrPtr = newInstrPtr, direction = R, trampoline = trampoline', ..}
  where
    trampoline' = False
    newInstrPtr = if x == width - 1 then (y, 0) else (y, x + stride)
    stride = if trampoline then 2 else 1
    (y, x) = instrPtr
move State {direction = L, trampoline, ..} = State {instrPtr = newInstrPtr, direction = L, trampoline = trampoline', ..}
  where
    trampoline' = False
    newInstrPtr = if x == 0 then (y, width - 1) else (y, x - stride)
    stride = if trampoline then 2 else 1
    (y, x) = instrPtr
move State {direction = U, trampoline, ..} = State {instrPtr = newInstrPtr, direction = U, trampoline = trampoline', ..}
  where
    trampoline' = False
    newInstrPtr = if y == 0 then (height - 1, x) else (y - stride, x)
    stride = if trampoline then 2 else 1
    (y, x) = instrPtr
move State {direction = D, trampoline, ..} = State {instrPtr = newInstrPtr, direction = D, trampoline = trampoline', ..}
  where
    trampoline' = False
    newInstrPtr = if y == height - 1 then (0, x) else (y + stride, x)
    stride = if trampoline then 2 else 1
    (y, x) = instrPtr
move State {direction = X, ..} = State {direction = X, instrPtr = newInstrPtr, gen = gen', ..}
  where
    State {instrPtr = newInstrPtr} = move State {direction = newDirection, ..}
    (newDirection, gen') = randomizeDirection gen

-- <pre>08>:1-:v v *_$.@
--   ^    _$>\:^</pre>
