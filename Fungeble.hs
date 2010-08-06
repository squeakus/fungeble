import System.Random
import Data.List  
import Data.Map (Map, member, (!), size, elemAt, fromList)
import Data.Set (Set, fromList)
import Debug.Trace


{-properties-}
defaultFitness = 100
popSize = 100
chromosomeSize = 5

{- Calls mutate on the population. Resets the individual since a
 change should occur. TODO (Could be smarter an verify if a reset is needed)-}
mutateOp :: Population -> [Float] -> [Int] -> Population
mutateOp [] _ _ = []
mutateOp (ind:pop) rndDs rndIs = (createIndiv (mutate'' (genotype ind) rndDs rndIs)) : mutateOp pop rndDs rndIs

{- Mutate a genotype by uniformly changing the integer. TODO The
 mutation value is hard coded -}
mutate'' :: [Int] -> [Float] -> [Int] -> [Int]
mutate'' [] _ _ = []
mutate'' _ [] _ = []
mutate'' _ _ [] = []
mutate'' (c:cs) (rndD:rndDs) (rndI:rndIs) = (if rndD > 0.1 then c else rndI) : mutate'' cs rndDs rndIs

{- Calls crossover on the population TODO How does it handle oddnumber
 sized populations? Fold? Smarter resetting values in individual-}
xoverOp :: Population -> [Float] -> Population
xoverOp [] _ = []
xoverOp (ind1:ind2:pop) rndDs = 
  let (child1, child2) = xover (genotype ind1,genotype ind2) rndDs
  in (createIndiv child1): (createIndiv child2) : xoverOp pop rndDs
xoverOp (ind1:[]) rndDs = [ind1]         

{- Singlepoint crossover, crossover porbability is hardcoded-}
xover :: ([Int], [Int]) -> [Float] -> ([Int], [Int])
xover ([],_) _ = ([],[])
xover (_,[]) _ = ([],[])
xover (_,_) [] = error "Empty rnd"
xover (p1,p2) (rndD:rndDs) =  
  if rndD > 0.7
     then let xopoint1 = xopoint rndDs p1; xopoint2 = xopoint rndDs p2
          in (take xopoint1 p1 ++ drop xopoint2 p2, take xopoint2 p2 ++ drop xopoint1 p1)
     else (p1, p2)
          
{- Utility function for getting crossover point TODO Catch errors -}
xopoint :: [Float] -> [Int] -> Int
xopoint (rnd:rndDs) codons = round $ (rnd) * (fromIntegral $ length codons)

{- Tournament selection on a population, counting the individuals via the cnt variable TODO Better recursion?-}
tournamentSelection :: Int -> Population -> [Int] -> Int -> Population
tournamentSelection 0 _ _ _ = []
tournamentSelection _ [] _ _ = error "Empty population"
tournamentSelection _ _ [] _ = error "Empty rnd"
tournamentSelection _ _ _ 0 = error "Zero tournament size" --What about minus?
tournamentSelection cnt pop rndIs tournamentSize = (maximumInd $ selectIndividuals rndIs pop tournamentSize) : tournamentSelection (cnt - 1) pop (drop tournamentSize rndIs) tournamentSize 

{-Selection with replacement TODO (Use parital application for tournament
selection and select individuals?-}
selectIndividuals :: [Int] -> Population -> Int -> Population
selectIndividuals _ _ 0 = [] 
selectIndividuals _ [] _ = error "Empty population"
selectIndividuals [] _ _ = error "Empty rnd"
selectIndividuals (rnd:rndIs) pop tournamentSize = (pop !! (rnd `mod` (length pop) ) ) : selectIndividuals rndIs pop (tournamentSize - 1)

{- Generational replacement with elites. TODO error catching-}
generationalReplacement :: Population -> Population -> Int -> Population
generationalReplacement orgPop newPop elites = (take elites $ sortBy sortInd orgPop) ++ (take (length newPop - elites) $ sortBy sortInd newPop)

{- Pattern matching two strings. If a position is unmatched 1 is added, matching strings return 0-}
patternMatch :: String -> String -> Int
patternMatch [] target = length target
patternMatch phenotype [] = length phenotype
patternMatch (p:phenotype) (f:target) = (if p /= f then 1 else 0) + patternMatch phenotype target

{- Pattern matches the population. String target is hardcoded-}
patternMatchOp :: Population -> Population
patternMatchOp [] = []
patternMatchOp (ind:pop) = (GEIndividual (genotype ind) (phenotype ind) (patternMatch (show (phenotype ind)) "aba") 0) : patternMatchOp pop

{- Mapping the entire population. Default fitness is hardcoded. TODO
 Wrapping is done by increasing the size of the input explicitly by
 (take (wraps * length genotype) (cycle genotypr)) -}
mappingOp :: Population -> BNFGrammar -> Population
mappingOp [] _ = []
mappingOp (ind:pop) grammar = (GEIndividual (genotype ind) (genotype2phenotype (genotype ind) [startSymbol grammar] grammar) 100 0) : mappingOp pop grammar
                                       
{-Makes an individual with default values-}
createIndiv :: [Int] -> GEIndividual
createIndiv [] = error "creating individual with an empty chromosome"
createIndiv xs = GEIndividual xs [] defaultFitness 0

{-creates an array of individuals with random genotypes-}
createPop :: Int -> [Int] -> Population
createPop 0 _ = []
createPop popCnt rndInts = createIndiv (take chromosomeSize rndInts) : createPop (popCnt-1) (drop chromosomeSize rndInts)
                           
{- Map genotype to phenotype (input to output) via the grammar TODO
 Make with derivation tree?-}
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
       
{- Evolve the population recursively counting with genptype and
returning a population of the best individuals of each
generation. Hard coding tournament size and elite size-}
evolve :: Population -> [Int] -> Int -> [Float] -> BNFGrammar -> Population
evolve pop _ 0 _ _ = []
evolve [] _ _ _ _ = error "Empty population"
evolve pop rndIs gen rndDs grammar = maximumInd pop : 
                                     evolve (
                                       generationalReplacement pop (
                                          patternMatchOp (
                                             mappingOp ( 
                                                mutateOp (
                                                   xoverOp (
                                                      tournamentSelection (length pop) pop rndIs 3) 
                                                   rndDs) 
                                                rndDs rndIs) 
                                             grammar)
                                          ) 
                                       1)
                                     rndIs (gen - 1) rndDs grammar
                                
{- Utility for sorting GEIndividuals-}
sortInd :: GEIndividual -> GEIndividual -> Ordering
sortInd ind1 ind2
  | fitness ind1 > fitness ind2 = GT
  | fitness ind1 < fitness ind2 = LT
  | fitness ind1 == fitness ind2 = EQ
                              
{- Utility for finding the maximum fitness in a Population TODO possible to shorten-}                           
maximumInd :: Population -> GEIndividual
maximumInd [] = error "Maximum of empty pop"
maximumInd [ind] = ind
maximumInd (ind:pop) = 
  let ind2 = maximumInd pop
  in if (fitness ind) > (fitness ind2)
     then ind
     else ind2

{- Testing the functions-}
main = do
  gen <- getStdGen
  let randNumber = randomRs (1,10) gen :: [Int]
  let randNumberD = randomRs (0,1) gen :: [Float]
  let cs = [100..104]
  print $ take 5 randNumberD
  print $ mutate'' cs randNumberD randNumber
  print $ xover (cs, [200..204]) randNumberD
  let pop = [createIndiv [3..10] , createIndiv [5..11]]
  print $ tournamentSelection (length pop) pop randNumber 3
  let newPop = [createIndiv [1..10], createIndiv [1..10]]
  print $ generationalReplacement pop newPop 2
  let ts = Data.Set.fromList ["a","b"]; nts = Data.Set.fromList ["S", "B"]; s = (NonTerminal "S")
  let grammar = (BNFGrammar ts nts (Data.Map.fromList [ ((NonTerminal "S"), [[(NonTerminal "S"), (NonTerminal "B")], [(NonTerminal "B")]]), ((NonTerminal "B"), [[(Terminal "a")],[(Terminal "b")]])]) s); wraps = 2
  let phen = genotype2phenotype (take (wraps * length cs) (cycle cs)) [startSymbol grammar] grammar
  print $ phen
  print $ patternMatch (show phen) "aa"
  print $ evolve pop randNumber 10 randNumberD grammar
  print $ "look at my pop!"
  print $ createPop 3 randNumber
{- Grammar used
S -> SB | B
B -> a | b
-}

{- Data for GEIndividual -}
--data Individual = GEIndividual [Int]  
--                  | GEIndividual [Int] [Symbol] 
--                  | GEIndividual [Int] [Symbol] Int
--                  | GEIndividual [Int] [Symbol] Int Int 
  
data GEIndividual = GEIndividual { genotype :: [Int]
                                 , phenotype :: [Symbol]
                                 , fitness :: Int
                                 , usedCodons :: Int
                                 } deriving (Show, Eq)
                                            
{- Type for population-}
type Population = [GEIndividual]

{-Type for terminal-}
type Terminal s = s
{- Type for Non Terminal-}
type NonTerminal s = s
{- Type for Production-}
type Production = [Symbol]
{-Data for symbol-}
data Symbol = Terminal String| NonTerminal String deriving (Show, Eq, Ord)
{- Data for BNF Grammar-}
data BNFGrammar = BNFGrammar {terminals :: Set (Terminal String)
                             , nonTerminals :: Set (NonTerminal String)
                             , rules :: Map (Symbol) [Production]
                             , startSymbol :: (Symbol)
                             } deriving (Show, Eq)


--TODO
--Generalise functions, use HOFs, e.g. operate function
--Functions for creating and reseting individuals
--Parse text file
--More operators
--Documentation
--Use Maybe
--Compare for GEIndividual
--Difference between type and data
--Show for symbols
--Minimize instead of Maximize
--Problem where code is generated
--Test parallelization
--Put op in front of generational and tournament
