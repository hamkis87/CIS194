{-# OPTIONS_GHC -Wall #-} 
module LogAnalysis where 


import Log


parseMessage :: String -> LogMessage
parseMessage message = let
                         parseWord ("I":x:xs) = LogMessage Info (read x) (unwords xs)
                         parseWord ("W":x:xs) = LogMessage Warning (read x) (unwords xs)
                         parseWord ("E":n:x:xs) = LogMessage (Error (read n)) (read x) (unwords xs)
                         parseWord xs = Unknown (unwords xs)
                       in
                         parseWord (words message)

parse :: String -> [LogMessage]
parse logFile = map parseMessage (lines logFile)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) message_tree = message_tree
insert log_message Leaf = Node Leaf log_message Leaf
insert log_message message_tree = let
                                    LogMessage _ ts _ = log_message
                                    Node l m r = message_tree
                                    LogMessage _ node_ts _ = m
                                  in
                                    case ts < node_ts of
                                         True -> Node (insert log_message l) m r
                                         False -> Node l m (insert log_message r)

build :: [LogMessage] -> MessageTree
build messages = let
                   build' [] msg_tree = msg_tree
                   build' (m:ms) msg_tree = build' ms (insert m msg_tree)
                 in
                   build' messages Leaf
