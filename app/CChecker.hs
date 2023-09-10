module CChecker where
import CParser
import Data.Maybe
import qualified Data.Map as M

check :: Program -> Maybe String
check (Program fs) = if checkMultDefs fs then Just "You tryna confuse me with all those redefinitions mate." else
                     if checkIncoDefs fs then Just "Make up your damn mind about those funtion definitions." else
                     Nothing

checkMultDefs :: [FunctionDecl] -> Bool
checkMultDefs fs = let fs' = map (\(FunctionDecl ((n, _, _), _)) -> n) $ filter (\(FunctionDecl ((_, _, b), _)) -> isJust b) fs in anySame fs'
                where anySame :: M.Map String () -> [String] -> Bool
                      anySame _ [] = False
                      anySame m (s:ss) = if M.member s m then True else anySame (m.insert s ()) ss

checkIncoDefs :: [FunctionDecl] -> Bool
checkIncoDefs fs = or (map f $ groupBy (\(FunctionDecl ((n1, _, _), _)) (FunctionDecl ((n2, _, _), _)) -> n1 == n2) fs)
                where f ds = allSame $ map (\(FunctionDecl ((_, a, _), _)) -> length a) ds
                      allSame :: [Int] -> Bool
                      allSame [] = False
                      allSame (a:as) = if a `elem` as then allSame as else False
