data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving (Show)

createBalancedTree :: [a] -> BinaryTree a
createBalancedTree [] = Empty
createBalancedTree xs = Node mid leftSubTree rightSubTree
  where
    (left, mid:right) = splitAt (length xs `div` 2) xs
    leftSubTree = createBalancedTree left
    rightSubTree = createBalancedTree right

countNodes :: BinaryTree a -> Int
countNodes Empty = 0
countNodes (Node _ left right) = 1 + countNodes left + countNodes right

heightOfTree :: BinaryTree a -> Int
heightOfTree Empty = 0
heightOfTree (Node _ left right) = 1 + max (heightOfTree left) (heightOfTree right)

main :: IO ()
main = do
    let elements = [1..10]
    putStrLn $ "Elements: " ++ show elements
    let tree = createBalancedTree elements
    putStrLn $ "Number of nodes in the tree: " ++ show (countNodes tree)
    putStrLn $ "Height of the tree: " ++ show (heightOfTree tree)
