import System.Random
import Data.List  
import Data.Map (Map, member, (!), size, elemAt, fromList)
import Data.Set (Set, fromList)
import Debug.Trace

-- Resets the individual since a change should occur (Could be smarter)
mutateOp :: Population -> [Float] -> [Int] -> Population
mutateOp [] _ _ = []
mutateOp (ind:pop) rndDs rndIs = (GEIndividual (mutate'' (genotype ind) rndDs rndIs) [] 100 0) : mutateOp pop rndDs rndIs

mutate'' :: [Int] -> [Float] -> [Int] -> [Int]
mutate'' [] _ _ = []
mutate'' _ [] _ = []
mutate'' _ _ [] = []
mutate'' (c:cs) (rndD:rndDs) (rndI:rndIs) = (if rndD > 0.1 then c else rndI) : mutate'' cs rndDs rndIs

-- Fold? Smarter resetting values in individual
xoverOp :: Population -> [Float] -> Population
xoverOp [] _ = []
xoverOp (ind1:ind2:pop) rndDs = 
  let (child1, child2) = xover (genotype ind1,genotype ind2) rndDs
  in (GEIndividual child1 [] 100 0): (GEIndividual child2 [] 100 0) : xoverOp pop rndDs
         
xover :: ([Int], [Int]) -> [Float] -> ([Int], [Int])
xover ([],_) _ = ([],[])
xover (_,[]) _ = ([],[])
xover (_,_) [] = ([],[])
xover (p1,p2) rndDs =  
  if (head $ tail rndDs) > 0.7
     then let xopoint1 = xopoint rndDs p1; xopoint2 = xopoint rndDs p2
          in (take xopoint1 p1 ++ drop xopoint1 p2, take xopoint2 p2 ++ drop xopoint2 p1)
     else (p1, p2)
          
xopoint :: [Float] -> [Int] -> Int
xopoint rndDs codons = round $ (head $ tail rndDs) * (fromIntegral $ length codons)

--How to count through the recursion?? Selection with replacement
tournamentSelection :: Int -> Population -> [Int] -> Int -> Population
tournamentSelection 0 _ _ _ = []
tournamentSelection _ [] _ _ = error "Empty population"
tournamentSelection _ _ [] _ = error "Empty rnd"
tournamentSelection _ _ _ 0 = error "Zero tournament size" --What about minus?
tournamentSelection cnt pop (_:rndIs) tournamentSize = (maximumInd $ selectIndividuals rndIs pop tournamentSize) : tournamentSelection (cnt - 1) pop rndIs tournamentSize 

--Selection with replacement (Use parital application for tournament
--selection and select individuals?? They are very similar
selectIndividuals :: [Int] -> Population -> Int -> Population
selectIndividuals _ _ 0 = [] 
selectIndividuals _ [] _ = error "Empty population"
selectIndividuals [] _ _ = error "Empty rnd"
selectIndividuals (rnd:rndIs) pop tournamentSize = (pop !! (rnd `mod` (length pop) ) ) : selectIndividuals rndIs pop (tournamentSize - 1)

generationalReplacement :: Population -> Population -> Int -> Population
generationalReplacement orgPop newPop elites = (take elites $ sortBy sortInd orgPop) ++ (take (length newPop - elites) $ sortBy sortInd newPop)

patternMatch :: String -> String -> Int
patternMatch [] facit = length facit
patternMatch phenotype [] = length phenotype
patternMatch (p:phenotype) (f:facit) = (if p /= f then 1 else 0) + patternMatch phenotype facit

patternMatchOp :: Population -> Population
patternMatchOp [] = []
patternMatchOp (ind:pop) = (GEIndividual (genotype ind) (phenotype ind) (patternMatch (show (phenotype ind)) "aba") 0) : patternMatchOp pop

mappingOp :: Population -> BNFGrammar -> Population
mappingOp [] _ = []
mappingOp (ind:pop) grammar = (GEIndividual (genotype ind) (genotype2phenotype (genotype ind) [startSymbol grammar] grammar) 100 0) : mappingOp pop grammar
                                       
--Make with derivation tree? Wrapping
genotype2phenotype :: [Int] -> [Symbol] -> BNFGrammar -> [Symbol]
genotype2phenotype [] _ _ = []
genotype2phenotype _ [] _ = []
genotype2phenotype (c:cs) (s:ss) grammar = 
  if (member s (rules grammar))
  then let rule = (rules grammar) ! s; sizeR = length rule;
    in --trace (show c ++ ":" ++ s ++ ":" ++ show rule ++ ":" ++ show ss ++ ":" ++ show cs) 
       genotype2phenotype (if sizeR > 1 then cs else (c:cs)) ( (rule !! (c `mod` sizeR) ) ++ ss) grammar 
  else --trace (show c ++ ":" ++ s) 
       s : genotype2phenotype (c:cs) ss grammar

evolve :: Population -> [Int] -> Int -> [Float] -> BNFGrammar -> Population
evolve pop _ 0 _ _ = []
evolve [] _ _ _ _ = error "Empty population"
evolve pop rndIs gen rndDs grammar = maximumInd pop : evolve (
                             generationalReplacement pop (
                                patternMatchOp (
                                   mappingOp ( 
                                      mutateOp (
                                         xoverOp (
                                            tournamentSelection (length pop) pop rndIs 3) 
                                         rndDs) 
                                      rndDs rndIs) 
                                   grammar)
                                ) 1)
                             rndIs (gen - 1) rndDs grammar
                                
sortInd :: GEIndividual -> GEIndividual -> Ordering
sortInd ind1 ind2
  | fitness ind1 < fitness ind2 = GT
  | fitness ind1 > fitness ind2 = LT
  | fitness ind1 == fitness ind2 = EQ
                              
maximumInd :: Population -> GEIndividual
maximumInd [] = error "Maximum of empty pop"
maximumInd [ind] = ind
maximumInd (ind:pop) = 
  let ind2 = maximumInd pop
  in if (fitness ind) > (fitness ind2)
     then ind
     else ind2

main = do
  gen <- getStdGen
  let randNumber = randomRs (1,10) gen :: [Int]
  let randNumberD = randomRs (0,1) gen :: [Float]
  let cs = [100..104]
  print $ take 5 randNumberD
  print $ mutate'' cs randNumberD randNumber
  print $ xover (cs, [200..204]) randNumberD
  let pop = [GEIndividual [1..10] [] 0 0, GEIndividual [2..11] [] 0 0]
  print $ tournamentSelection (length pop) pop randNumber 3
  let newPop = [GEIndividual [1..10] [] 0 0, GEIndividual [2..11] [] 0 0]
  print $ generationalReplacement pop newPop 2
  let ts = Data.Set.fromList ["a","b"]; nts = Data.Set.fromList ["S", "B"]; s = (NonTerminal "S")
  let grammar = (BNFGrammar ts nts (Data.Map.fromList [ ((NonTerminal "S"), [[(NonTerminal "S"), (NonTerminal "B")], [(NonTerminal "B")]]), ((NonTerminal "B"), [[(Terminal "a")],[(Terminal "b")]])]) s); wraps = 2
  let phen = genotype2phenotype (take (wraps * length cs) (cycle cs)) [startSymbol grammar] grammar
  print $ phen
  print $ patternMatch (show phen) "aa"
  print $ evolve pop randNumber 10 randNumberD grammar

{- 
S -> SB | B
B -> a | b
-}

data GEIndividual = GEIndividual { genotype :: [Int]
                                 , phenotype :: [Symbol]
                                 , fitness :: Int
                                 , usedCodons :: Int
                                 } deriving (Show, Eq)

type Population = [GEIndividual]

type Terminal s = s
type NonTerminal s = s
type Production = [Symbol]
data Symbol = Terminal String| NonTerminal String deriving (Show, Eq, Ord)

data BNFGrammar = BNFGrammar {terminals :: Set (Terminal String)
                             , nonTerminals :: Set (NonTerminal String)
                             , rules :: Map (Symbol) [Production]
                             , startSymbol :: (Symbol)
                             } deriving (Show, Eq)

--TODO
--Generalise functions, use HOFs, e.g. operat function
--Parse text file
--More operators
--Documentation
--Use Maybe