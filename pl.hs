import System.Random


---------- Trees

data Tree = Leaf Int | Node Int Tree Tree

instance Show Tree where
  show (Leaf i) = show i
  show (Node i s t) = show' s ++ [['a'..'z'] !! i] ++ show' t
    where
      show' (Leaf i) = show i
      show' (Node i s t) = "(" ++ show' s ++ [['a'..'z'] !! i] ++ show' t ++ ")"

---------- Tree functions

newleaves :: Tree -> Tree
newleaves = snd . f 1 where
  f i (Leaf _) = (i+1,Leaf i)
  f i (Node n s t) = let (j,s') = f i s; (k,t') = f j t in (k, Node n s' t')

mapnodes :: (Int -> Int) -> Tree -> Tree
mapnodes f (Leaf x) = Leaf x
mapnodes f (Node i s t) = Node (f i) (mapnodes f s) (mapnodes f t)

---------- Tree properties

size :: Tree -> Int
size (Leaf _) = 0
size (Node _ s t) = 1 + size s + size t

ordered :: Tree -> Bool
ordered (Leaf _) = True
ordered (Node i s t) = f i s && f i t
  where
    f _ (Leaf _) = True
    f i (Node j s t) = f j s && f j t && i <= j

once :: Tree -> Bool
once = f []
 where
  f _ (Leaf _) = True
  f is (Node i s t) = not (elem i is) && f (i:is) s && f (i:is) t



---------- Generating trees

tree :: Int -> Tree
tree i = newleaves (f [i-1,i-2..0])
  where
    f [] = Leaf 0
    f (x:xs) = let (ys,zs) = everyother xs in Node x (f ys) (f zs) 
    everyother [] = ([],[])
    everyother [x] = ([x],[])
    everyother (x:y:zs) = let (xs,ys) = everyother zs in (x:xs,y:ys)

lineleft :: Int -> Tree
lineleft = newleaves . f where
  f i = if i <= 0 then Leaf 0 else Node (i-1) (f (i-1)) (Leaf 0)

lineright :: Int -> Tree
lineright = newleaves . f where
  f i = if i <= 0 then Leaf 0 else Node (i-1) (Leaf 0) (f (i-1))

fork :: Int -> Tree
fork 0 = Leaf 1
fork i = let j = div i 2 in newleaves $
  Node (i-1) (mapnodes ((*2)) $ lineleft j) (mapnodes ((+1).(*2)) $ lineright (i-j-1))



---------- Reduction steps

step :: Tree -> [([Bool],Tree)]
step (Leaf _) = []
step (Node i s t) =
  [ (False:as,Node i s' t) | (as,s') <- step s ] ++
  [ (True :bs,Node i s t') | (bs,t') <- step t ] ++
  g where
    g | Node j u v <- s, j < i, Node k x y <- t, k < i =
         if j < k
         then [([False],Node j (Node i u t) (Node i v t)),([True] ,Node k (Node i s x) (Node i s y))]
         else [([True] ,Node k (Node i s x) (Node i s y)),([False],Node j (Node i u t) (Node i v t))]
      | Node j u v <- s, j < i = [([False],Node j (Node i u t) (Node i v t))]
      | Node k x y <- t, k < i = [([True] ,Node k (Node i s x) (Node i s y))]
      | otherwise = []

stepbest :: Tree -> [Tree]
stepbest (Leaf _) = []
stepbest (Node i s t) =
  [ Node i s' t | s' <- stepbest s ] ++
  [ Node i s t' | t' <- stepbest t ] ++
  g where
    g | Node j u v <- s, j < i, Node k x y <- t, k < i = if j < k then [Node j (Node i u t) (Node i v t)] else [Node k (Node i s x) (Node i s y)]
      | Node j u v <- s, j < i = [Node j (Node i u t) (Node i v t)]
      | Node k x y <- t, k < i = [Node k (Node i s x) (Node i s y)]
      | otherwise = []

stepworst :: Tree -> [Tree]
stepworst (Leaf _) = []
stepworst (Node i s t) =
  g ++
  [ Node i s' t | s' <- stepworst s ] ++
  [ Node i s t' | t' <- stepworst t ]
 where
    g | Node j u v <- s, j < i, Node k x y <- t, k < i = if j < k then [Node k (Node i s x) (Node i s y)] else [Node j (Node i u t) (Node i v t)]
      | Node j u v <- s, j < i = [Node j (Node i u t) (Node i v t)]
      | Node k x y <- t, k < i = [Node k (Node i s x) (Node i s y)]
      | otherwise = []

---------- Reduction

best :: Tree -> Tree
best t = case stepbest t of (s:_) -> best s; [] -> t

worst :: Tree -> Tree
worst t = case stepworst t of (s:_) -> worst s; [] -> t

auto :: Tree -> IO Tree
auto t = do
  print t
  let ts = step t
  if null ts then return t else do
    n <- randomIO 
    auto $ snd (ts !! (n `mod` length ts))

manual :: Tree -> IO Tree
manual t = do
  print t
  let ts = step t
  if null ts then return t else do
    sequence_ [ putStrLn (show i ++ ": " ++ map (\b -> if b then '1' else '0') a ++ " => " ++ show s) | (i,(a,s)) <- zip [0..] ts]
    n <- getLine
    if n == "a" then auto t
    else if n == "x" then return t
    else manual $ snd (ts !! (read n `mod` length ts))


