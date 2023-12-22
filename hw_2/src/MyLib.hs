module MyLib(someFunc) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Определение функции reverseList, которая разворачивает список.
reverseList :: [a] -> [a]
reverseList [] = []     -- Пустой список остается пустым.
reverseList (x:xs) = xs -- Голова списка убирается, оставляя хвост.

-- Определение типа данных для бинарного дерева.
data BinaryTree a
  = EmptyTree            
  | TreeNode
    { leftSubtree  :: Maybe (BinaryTree a)  
    , nodeValue    :: a                      
    , rightSubtree :: Maybe (BinaryTree a) 
    }
  deriving (Eq, Show, Read)

-- Создание пустого дерева.
emptyTree :: BinaryTree a
emptyTree = EmptyTree

-- Создание узла с заданным значением.
leafNode :: a -> BinaryTree a
leafNode a = TreeNode Nothing a Nothing

-- Функция для обхода бинарного дерева в порядке in-order.
traverseTree :: BinaryTree a -> [a]
traverseTree EmptyTree = [] 
traverseTree (TreeNode leftSubtree value rightSubtree)
  = maybe [] traverseTree leftSubtree         
    ++ [value]                                
    ++ maybe [] traverseTree rightSubtree     

-- Функция для вставки значения в бинарное дерево.
insertNode :: Ord a => a -> BinaryTree a -> BinaryTree a
insertNode v EmptyTree = leafNode v
insertNode v tree@(TreeNode leftSubtree rootValue rightSubtree)
  | v < rootValue  = tree{ leftSubtree = Just $ maybe (leafNode v) (insertNode v) leftSubtree }
  | otherwise      = tree{ rightSubtree = Just $ maybe (leafNode v) (insertNode v) rightSubtree }

-- Функция для поворота дерева влево.
rotateTreeLeft :: BinaryTree a -> BinaryTree a
rotateTreeLeft (TreeNode a x (Just (TreeNode b y c))) = TreeNode (Just (TreeNode a x b)) y c

-- Функция для поворота дерева вправо.
rotateTreeRight :: BinaryTree a -> BinaryTree a
rotateTreeRight (TreeNode (Just (TreeNode a x b)) y c) = TreeNode a x (Just (TreeNode b y c))