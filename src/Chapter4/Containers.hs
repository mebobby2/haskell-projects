module Chapter4.Containers where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tree
import Data.Graph

-- M.fromList [("hello", 3), ("bye", 4)]

insert' :: Ord k => k -> a -> M.Map k a -> M.Map k a
insert' k v = M.alter (\_ -> Just (v)) k

delete' :: Ord k => k -> M.Map k a -> M.Map k a
delete' k = M.alter (\_ -> Nothing) k

pictureTree :: Tree Int
pictureTree = Node 1 [ Node 2 [ Node 3 []
                              , Node 4 []
                              , Node 5 [] ]
                      , Node 6 [] ]

preOrderDepthFirst :: (a -> b) -> Tree a -> [b]
preOrderDepthFirst f (Node v subtrees) = let subtreesTraversed = concat $ map (preOrderDepthFirst f) subtrees
                                in f v : subtreesTraversed

--preOrder show pictureTree

breathFirst :: Tree a -> [a]
breathFirst = concat . levels

timeMachineGraph :: [(String, String, [String])]
timeMachineGraph =
  [("wood","wood",["walls"]), ("plastic","plastic",["walls","wheels"])
  ,("aluminum","aluminum",["wheels","door"]),("walls","walls",["done"])
  ,("wheels","wheels",["done"]),("door","door",["done"]),("done","done",[])]

timeMachinePrecedence :: (Graph, Vertex -> (String,String,[String]), String -> Maybe Vertex)
timeMachinePrecedence = graphFromEdges timeMachineGraph

timeMachineDevPlan :: (Graph, Vertex -> (String,String,[String]), String -> Maybe Vertex) -> [String]
timeMachineDevPlan (g,v,_) = map (\x -> let (k,_,_) = v x in k) $ topSort g
--timeMachineDevPlan timeMachinePrecedence

timeMachineTravel :: Graph
timeMachineTravel = buildG (103,2013)
                      [(1302,1614),(1614,1302),(1302,2013),(2013,1302),(1614,2013)
                      ,(2013,1408),(1408,1993),(1408,917),(1993,917),(907,103),(103,917)]


--path timeMachineTravel 1302 917
--reachable timeMachineTravel 1302