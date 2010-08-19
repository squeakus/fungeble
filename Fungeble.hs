import Random
import Control.Monad.State
import Monad
import System.Random
import Data.List  
import Data.Map (Map, member, (!), size, elemAt, fromList)
import Data.Set (Set, fromList)
import Debug.Trace


{-properties-}
defaultFitness = 100000
popSize = 100
generations = 100
chromosomeSize = 100
mutationRate = 0.01
crossoverRate = 0.7
patternMatchTarget = "abababab"
tournamentSize = 3
eliteSize = 1

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

{- Calls mutate on the population. Resets the individual since a
 change should occur. TODO (Could be smarter an verify if a reset is needed)-}
mutateOp :: Population -> [Float] -> [Int] -> Population
mutateOp [] _ _ = []
mutateOp (ind:pop) rndDs rndIs = (createIndiv (mutate'' (genotype ind) (drop (length (genotype ind)) rndDs) (drop (length (genotype ind)) rndIs))) : mutateOp pop rndDs rndIs

{- Mutate a genotype by uniformly changing the integer. TODO The
 mutation value is hard coded -}
mutate'' :: [Int] -> [Float] -> [Int] -> [Int]
mutate'' [] _ _ = []
mutate'' _ [] _ = []
mutate'' _ _ [] = []
mutate'' (c:cs) (rndD:rndDs) (rndI:rndIs) = --trace("mu:" ++ show rndD ++ ">" ++ show mutationRate)
                                            (if rndD > mutationRate then c else rndI) : mutate'' cs rndDs rndIs

{- Calls crossover on the population TODO How does it handle oddnumber
 sized populations? Fold? Smarter resetting values in individual TODO hardcoding rnd drop-}
xoverOp :: Population -> [Float] -> Population
xoverOp [] _ = []
xoverOp (ind1:ind2:pop) rndDs = 
  let (child1, child2) = xover (genotype ind1,genotype ind2) (take 3 rndDs)
  in (createIndiv child1): (createIndiv child2) : xoverOp pop (drop 3 rndDs)
xoverOp (ind1:[]) rndDs = [ind1]         

{- Singlepoint crossover, crossover porbability is hardcoded-}
xover :: ([Int], [Int]) -> [Float] -> ([Int], [Int])
xover ([],_) _ = ([],[])
xover (_,[]) _ = ([],[])
xover (_,_) [] = error "Empty rnd"
xover (p1,p2) (rndD:rndDs) =  
  if rndD < crossoverRate
     -- Remove the used random values for the rndDs for the xopoints calls
     then let xopoint1 = xopoint rndDs p1; xopoint2 = xopoint (drop 1 rndDs) p2
          in --trace ("xo:" ++ show rndD ++ ">" ++ show crossoverRate ++ show (take 3 rndDs))
           (take xopoint1 p1 ++ drop xopoint2 p2, take xopoint2 p2 ++ drop xopoint1 p1)
     else (p1, p2)
          
{- Utility function for getting crossover point TODO Make nicerway of returning 1 as a minimum value -}
xopoint :: [Float] -> [Int] -> Int
xopoint [] _ = error "Empty rnd"
xopoint _ [] = error "Empty genotype" 
xopoint (rnd:rndDs) codons = max 1 (round $ (rnd) * (fromIntegral $ length codons))

{- Tournament selection on a population, counting the individuals via the cnt variable TODO Better recursion?-}
tournamentSelectionOp :: Int -> Population -> [Int] -> Int -> Population
tournamentSelectionOp 0 _ _ _ = []
tournamentSelectionOp _ [] _ _ = error "Empty population"
tournamentSelectionOp _ _ [] _ = error "Empty rnd"
tournamentSelectionOp _ _ _ 0 = error "Zero tournament size" --What about minus?
tournamentSelectionOp cnt pop rndIs tournamentSize = (bestInd (selectIndividuals rndIs pop tournamentSize) minInd) : tournamentSelectionOp (cnt - 1) pop (drop tournamentSize rndIs) tournamentSize 

{-Selection with replacement TODO (Use parital application for tournament
selection and select individuals?-}
selectIndividuals :: [Int] -> Population -> Int -> Population
selectIndividuals _ _ 0 = [] 
selectIndividuals _ [] _ = error "Empty population"
selectIndividuals [] _ _ = error "Empty rnd"
selectIndividuals (rnd:rndIs) pop tournamentSize = (pop !! (rnd `mod` (length pop) ) ) : selectIndividuals rndIs pop (tournamentSize - 1)

{- Generational replacement with elites. TODO error catching-}
generationalReplacementOp :: Population -> Population -> Int -> Population
generationalReplacementOp orgPop newPop elites = 
  let pop = (take elites $ sortBy sortInd orgPop ) ++ (take (length newPop - elites) $ sortBy sortInd newPop )
  in --trace (showPop orgPop ++ "\n" ++ showPop newPop ++ "\n" ++ showPop pop ++ "\n")
     pop

showInd :: GEIndividual -> String
--showInd (GEIndividual genotype phenotype fitness usedCodons) = show genotype ++ ":" ++ show fitness
showInd (GEIndividual genotype phenotype fitness usedCodons) = show fitness

showPop :: Population -> String
showPop [] = ""
showPop (ind:pop) = showInd ind ++ ":" ++ showPop pop

{- Pattern matching two strings. If a position is unmatched 1 is added, matching strings return 0-}
patternMatch :: String -> String -> Int
patternMatch [] target = length target
patternMatch phenotype [] = length phenotype
patternMatch (p:phenotype) (f:target) = --trace (show [p]++[f]) 
                                        (if p /= f then 1 else 0) + patternMatch phenotype target

{- Pattern matches the population. String target is hardcoded-}
patternMatchOp :: Population -> Population
patternMatchOp [] = []
patternMatchOp (ind:pop) = --trace(show "matching indiv") 
                           (GEIndividual (genotype ind) (phenotype ind) (patternMatch (phenotype2string (phenotype ind)) patternMatchTarget) 0) : patternMatchOp pop

{- Mapping the entire population. Default fitness is hardcoded. TODO
 Wrapping is done by increasing the size of the input explicitly by
 (take (wraps * length genotype) (cycle genotypr)) -}
mappingOp :: Population -> BNFGrammar -> Population
mappingOp [] _ = []
mappingOp (ind:pop) grammar = (GEIndividual (genotype ind) (genotype2phenotype (genotype ind) [startSymbol grammar] grammar) defaultFitness 0) : mappingOp pop grammar
                                       
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
    in --trace (show c ++ ":" ++ show s ++ ":" ++ show rule ++ ":" ++ show ss ++ ":" ++ show cs) 
       genotype2phenotype (if sizeR > 1 then cs else (c:cs)) ( (rule !! (c `mod` sizeR) ) ++ ss) grammar 
  else --trace (show c ++ ":" ++ show s) 
       s : genotype2phenotype (c:cs) ss grammar

{- Evolve the population recursively counting with genptype and
returning a population of the best individuals of each
generation. Hard coding tournament size and elite size TODO drop a less arbitrary value of random values than 10-}
evolve :: Population -> [Int] -> Int -> [Float] -> BNFGrammar -> Population
evolve pop _ 0 _ _ = []
evolve [] _ _ _ _ = error "Empty population"
evolve pop rndIs gen rndDs grammar = bestInd pop minInd : 
                                     evolve (
                                       generationalReplacementOp pop (
                                          patternMatchOp (
                                             mappingOp ( 
                                                mutateOp (
                                                   xoverOp (
                                                      tournamentSelectionOp (length pop) pop rndIs tournamentSize) 
                                                   rndDs) 
                                                rndDs rndIs) 
                                             grammar)
                                          ) 
                                       eliteSize)
                                     (drop (popSize * 10) rndIs) (gen - 1) (drop (popSize * 10) rndDs) grammar
                                
{- Utility for sorting GEIndividuals-}
sortInd :: GEIndividual -> GEIndividual -> Ordering
sortInd ind1 ind2
  | fitness ind1 > fitness ind2 = GT
  | fitness ind1 < fitness ind2 = LT
  | fitness ind1 == fitness ind2 = EQ
                              
{- Utility for finding the maximum fitness in a Population-}                           
maxInd :: GEIndividual -> GEIndividual -> GEIndividual
maxInd ind1 ind2 
  | fitness ind1 > fitness ind2 = ind1
  | otherwise = ind2
{- Utility for finding the minimum fitness in a Population-}                           
minInd :: GEIndividual -> GEIndividual -> GEIndividual
minInd ind1 ind2 
  | fitness ind1 < fitness ind2 = ind1
  | otherwise = ind2
                
bestInd :: Population -> (GEIndividual -> GEIndividual -> GEIndividual) -> GEIndividual
bestInd (ind:pop) best = foldr best ind pop

phenotype2string :: [Symbol] -> String
phenotype2string [] = ""
phenotype2string (symbol:symbols) = symbol2string symbol ++ phenotype2string symbols
  
symbol2string :: Symbol -> String
symbol2string (Terminal s) = s
symbol2string (NonTerminal s) = s
  
{- Testing the functions-}
main = do
  gen <- getStdGen
  let randNumber = randomRs (1,127) gen :: [Int]
  let randNumberD = randomRs (0,1) gen :: [Float]
  let cs = [100..104]
  print $ take 5 randNumberD
  --print $ mutate'' cs randNumberD randNumber
  --print $ xover (cs, [200..204]) randNumberD
  let pop = createPop popSize randNumber
--  let pop = [createIndiv [0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1] , createIndiv [5..11]]
--  print $ tournamentSelectionOp (length pop) pop randNumber 3
  let newPop = [createIndiv [1..10], createIndiv [1..10]]
--  print $ generationalReplacementOp pop newPop 2
  let ts = Data.Set.fromList ["a","b"]; nts = Data.Set.fromList ["S", "B"]; s = (NonTerminal "S")
  let grammar = (BNFGrammar ts nts (Data.Map.fromList [ ((NonTerminal "S"), [[(NonTerminal "S"), (NonTerminal "B")], [(NonTerminal "B")]]), ((NonTerminal "B"), [[(Terminal "a")],[(Terminal "b")]])]) s); wraps = 2
  let bestInds = (evolve pop randNumber generations randNumberD grammar) 
--  print $ bestInds
  print $ bestInd bestInds minInd
--  print "Done"