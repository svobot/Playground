{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import           Log

parseMessage :: String -> LogMessage
parseMessage s = case words s of
        ("I" : time : content) -> LogMessage Info (read time) (unwords content)
        ("W" : time : content) ->
                LogMessage Warning (read time) (unwords content)
        ("E" : level : time : content) ->
                LogMessage (Error $ read level) (read time) (unwords content)
        content -> Unknown $ unwords content

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg         Leaf = Node Leaf msg Leaf
insert newMsg@(LogMessage _ newTime _) (Node t1 existingMsg@(LogMessage _ existingTime _) t2)
        | newTime <= existingTime
        = Node (insert newMsg t1) existingMsg t2
        | otherwise
        = Node t1 existingMsg (insert newMsg t2)
insert _ _ = undefined -- matches nonexistent case of Unknown log message in the tree node

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf             = []
inOrder (Node t1 msg t2) = inOrder t1 ++ [msg] ++ inOrder t2

errorWithSeverity :: Int -> LogMessage -> Bool
errorWithSeverity n (LogMessage (Error severity) _ _) = severity >= n
errorWithSeverity _ _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong =
        map (\(LogMessage _ _ msg) -> msg)
                . filter (errorWithSeverity 50)
                . inOrder
                . build
