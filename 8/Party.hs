module Party where
import           Data.Tree
import           Employee

glCons :: Employee -> GuestList -> GuestList
glCons e@Emp{empFun = fe} (GL xs fun) = GL (xs ++ [e]) (fun + fe)

instance Semigroup GuestList where
  (GL xs1 f1) <> (GL xs2 f2) = GL (xs1 ++ xs2) (f1 + f2)

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun a b = if a >= b then a else b

treeFold :: (a -> [b] -> b) -> [b] -> Tree a -> b
treeFold f init0 t@(Node root []) = f root init0
treeFold f init0 t@(Node root xs) = f root $ treeFold f init0 <$> xs

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e xs = (glCons e maxGLW, maxGL)
  where
    maxGL = maximum $ map fst xs
    maxGLW = maximum $ map snd xs

maxFun :: Tree Employee -> GuestList
maxFun = uncurry max . treeFold nextLevel [mempty]

main = readFile "company.txt" >>= print . maxFun . read
