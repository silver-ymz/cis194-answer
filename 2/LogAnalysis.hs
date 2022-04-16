module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s = case words s of
  ("I" : t : msg) -> LogMessage Info (read t) (unwords msg)
  ("W" : t : msg) -> LogMessage Warning (read t) (unwords msg)
  ("E" : e : t : msg) -> LogMessage (Error (read e)) (read t) (unwords msg)
  _ -> Unknown s

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert msg@LogMessage {} Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ t _) (Node l msg2@(LogMessage _ t' _) r)
  | t < t' = Node (insert msg l) msg2 r
  | otherwise = Node l msg2 (insert msg r)
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l msg r) = inOrder l ++ msg : inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lmsgList = (\(LogMessage _ _ s) -> s) <$> filter isWrong (inOrder $ build lmsgList)
  where
    isWrong (LogMessage (Error im) _ _) | im >= 50 = True
    isWrong _ = False

main :: IO ()
main = testParse parse 5000 "sample.log" >>= print . whatWentWrong
