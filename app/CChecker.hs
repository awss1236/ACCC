module CChecker where
import CParser
import Data.Maybe
import qualified Data.Map as M

check :: Program -> Maybe String
check (Program ts) = if checkMultDefs toDef ts then Just "You tryna confuse me with all those redefinitions mate." else
                     if checkIncoDefs toLs ts then Just "Make up your damn mind about those funtion definitions." else
                     Nothing
                     where
                       toDef (Func (FunctionDecl ((n, _, b), _))) = (n, isJust b)
                       toDef (GVar (Declare ((n, _), _))) = (n, True)
                       toLs  (Func (FunctionDecl ((n, a, _), _))) = (n, length a)
                       toLs  (GVar (Declare ((n, _), _))) = (n, 1)

checkMultDefs :: (a -> (String, Bool)) -> [a] -> Bool
checkMultDefs f fs = let fs' = map (fst.f) $ filter (snd.f) fs in anySame (M.empty) fs'
                where anySame :: M.Map String () -> [String] -> Bool
                      anySame _ [] = False
                      anySame m (s:ss) = if M.member s m then True else anySame (M.insert s () m) ss

checkIncoDefs :: (a -> (String, Int)) -> [a] -> Bool
checkIncoDefs f fs = or (map c $ groupBy (\a b -> (fst.f) a == (fst.f) b) fs)
                where c ds = allSame $ map (snd.f) ds
                      allSame :: [Int] -> Bool
                      allSame [] = False
                      allSame (a:as) = if a `elem` as then allSame as else False
                      groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
                      groupBy f as = foldr (ins f) [] as
                      ins :: (a -> a -> Bool) -> a -> [[a]] -> [[a]]
                      ins _ a [] = [[a]]
                      ins f a ((b:r):r') = if f a b then ((a:b:r):r') else ((b:r):(ins f a r'))
