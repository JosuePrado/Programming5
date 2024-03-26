module SwapNodes where


    data BTree a =  Empty
                    | Null
                    | Node a (BTree a) (BTree a)
                    deriving (Show, Eq) 

    tree :: BTree Int
    tree = Node 1 (Node 2 (Node 4 (Node 6 Empty Empty) Null) Null) (Node 3 (Node 5 (Node 7 Empty Empty) (Node 8 Empty Empty)) Null)

    initTree :: BTree Int
    initTree = Node 1 (Node 2 (Node 4 Empty Empty) (Node 1 Empty Empty)) (Node 3 (Node 5 Empty Empty) (Node 1 Empty Empty))

    addNode :: Int -> Int -> Int -> BTree Int -> BTree Int
    addNode a b _ (Node x Empty Empty) = Node x (Node a Empty Empty) (Node b Empty Empty)
    addNode a b n (Node x left right)   | haveChills left = Node x (addNode a b 1 left) right
                                        | haveChills right = Node x left (addNode a b 2 right)

    haveChills :: BTree Int -> Bool
    haveChills (Node x Empty Empty) = False
    haveChills (Node x left right) = True