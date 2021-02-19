data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b 
unfold f a = case f a of 
            Nothing -> Leaf
            Just (a,b,c) -> Node (unfold f a) b (unfold f c)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold treeFunction 0 where 
                            treeFunction = \x -> if  x==n then Nothing else Just((x+1),x,(x+1))  