-- |This module implements a translation from the Carneades argumentation model
-- into Dung's argumentation frameworks. Any cycle-free Carneades Argument
-- Evaluation Structure (CAES) is handled. We also give a Haskell implementation of 
-- correspondence properties. 
-- 
-- Translation is done according to the following algorithm (see also \"Towards a
-- framework for the implementation and verification of translations between 
-- argumentation models\" by Bas van Gijzel and Henrik Nilsson)
-- 
-- 1. generatedArgs = /emptyset/.
-- 
-- 2. sortedArgs = Topological sort of arguments on its dependency graph.
-- 
-- 3. while sortedArgs != /emptyset/: 
--  
--   * Pick the first argument in sortedArgs.
--     Remove all arguments from sortedArgs that have the same conclusion,
--     c, and put them in argSet. 
-- 
--   * Translate applicability part of arguments argSet, building on previously
--     generatedArgs and put the generated arguments in tempArgs.
--
--   * argSet = /emptyset/
-- 
--   * Repeat the above three steps for the arguments for the opposite conclusion. 
--
--   * Translate the acceptability part of c and the opposite conclusion based on 
--     arguments in tempArgs. Add the results and tempArgs to generatedArgs.
--   
--   * tempArgs = /emptyset/
module Language.CarneadesIntoDung.Translation
 (
    -- * Basic types
    ConcreteArg, LConcreteArg,
    ConcreteAF, LConcreteAF,
    -- * Translation functions
    translate, translate', 
    -- * Correspondence properties
    -- | Informally, the correspondence properties below state that every 
    -- argument and proposition in a CAES, after translation, will have a
    -- corresponding argument and keep the same acceptability status. 
    --
    -- If the translation function is a correct implementation, the Haskell 
    -- implementation of the correspondence properties should always return 
    -- 'True'. However to constitute an actual (mechanised) proof we would 
    -- need to convert the translation and the implementation of the 
    -- correspondence properties in Haskell to a theorem prover like Agda.
    --
    -- See Section 4.4 of the paper for the formally stated properties.
 

    corApp, corAcc)
 where
import Language.Dung.AF (DungAF(..), groundedExt)
import Language.Carneades.CarneadesDSL
import Language.Carneades.Cyclic
import Data.Graph.Inductive
import Data.List (find, delete, intersect)
import Data.Maybe (fromMaybe)
import Data.Either (lefts, rights)
import Data.Set (fromList)
import Prelude hiding (negate)

-- |A concrete argument (in an argumentation framework) is either a Carneades 
-- propositional literal, or a Carneades argument.
type ConcreteArg = Either PropLiteral Argument   

-- |A labelled version of the concrete argument allowing a more efficient 
-- translation by keeping track of the translation status.
type LConcreteArg = (Bool, ConcreteArg) 

-- |An argumentation framework (AF) instantiated with 'ConcreteArg'.
type ConcreteAF = DungAF ConcreteArg

-- |An argumentation framework (AF) instantiated with 'LConcreteArg'.
type LConcreteAF = DungAF LConcreteArg

-- |Assumed true argument in the translated AF. It is used to attack arguments
-- that do not uphold their proof standard or have unacceptable premises.
defeater :: LConcreteArg 
defeater = (True, Left $ mkProp "defeater")


-- | Topological sort of the dependency graph
-- The result is a list, pairing a proposition with all its pro arguments
topSort :: ArgSet -> [(PropLiteral, [Argument])]
topSort g | cyclic g  = error "Argumentation graph is cyclic!"
          | otherwise = reverse $ topsort' g

-- |Transforms a Carneades proposition into a Dung argument and labels it 'True'.
propToLArg :: PropLiteral -> LConcreteArg
propToLArg p = (True, Left p)

-- |Strips the label of both the 'LConcreteArg's in the attack.
stripAttack :: (LConcreteArg, LConcreteArg) -> (ConcreteArg, ConcreteArg) 
stripAttack (a, b) = (snd a, snd b)

-- |Translation function. It translate an arbitrary /cycle-free/ Carneades argument
-- Evaluation Structure (CAES) into a Dung argumentation framework (instantiated
-- with a ConcreteArg)
translate :: CAES -> ConcreteAF
translate caes@(CAES (argSet, (assumptions, _), _)) 
 = AF (map snd args) (map stripAttack attacks)
 where AF args attacks = argsToAF (topSort argSet) 
                                  caes 
                                  (AF (defeater : map propToLArg assumptions) [])


-- |Mainly, for testing purposes. This function behaves exactly like 'translate', 
-- but retains the labels. 
translate' :: CAES -> LConcreteAF 
translate' caes@(CAES (argSet, (assumptions, _), _))
 = AF args attacks
 where AF args attacks = argsToAF (topSort argSet) 
                                  caes 
                                  (AF (defeater : map propToLArg assumptions) [])


-- |Retrieves the arguments con the given proposition 'p'. 
conArgs :: PropLiteral -> [(PropLiteral, [Argument])] -> (PropLiteral, [Argument])
conArgs p argList = fromMaybe (negate p, []) (find ((== negate p) . fst) argList)

-- |Corresponds to the whole of 3. of the above algorithm (or Algorithm 4.1 in 
-- the paper)
-- 
-- If there are no more arguments to process, the translated AF is returned. 

-- If there is a propositional literal left, but it is an assumption, it has
-- already been translated and does not need to be considered. 
-- 
-- Otherwise, collect all pro and con arguments for p (con arguments are obtained
-- by calling 'conArgs') and remove them from @argList@. The translation is then 
-- done in four steps. 'transApps' is called to translate the applicability part of 
-- the pro and con arguments. 'transAcc' is called to translate the acceptability of 
-- p and the opposite of p (note that the order of applicable arguments is switched
-- for translating the acceptability of the opposite of p). The results of these 
-- four calls are collected and used in the recursive step of 'argsToAF'. 
argsToAF :: [(PropLiteral, [Argument])] -> CAES -> LConcreteAF -> LConcreteAF
argsToAF [] _ transAF = transAF
argsToAF (pro@(p, _proArgs) : argList) caes@(CAES (_, (assumptions, _), _)) (AF args defs)
 | p `elem` assumptions = argsToAF argList caes (AF args defs)
 | otherwise = 
 let  con                    = conArgs p argList
      (proAppArgs, proDefs)  = transApps args pro
      (conAppArgs, conDefs)  = transApps args con
      (newArgPro, proDefs')  = transAcc p proAppArgs conAppArgs caes
      (newArgCon, conDefs')  = transAcc (negate p) conAppArgs proAppArgs caes
      argList'               = delete con argList
 in argsToAF argList' caes 
            (AF (newArgPro : newArgCon : proAppArgs ++ conAppArgs ++ args) 
                (proDefs' ++ conDefs' ++ proDefs ++ conDefs ++ defs))

-- |Filters out propositional literals that have been labelled 'True'.
accProps :: [LConcreteArg] -> [PropLiteral]
accProps []                     = []
accProps ((True, Left p) : ls)  = p : accProps ls
accProps ((True, Right _) : ls) = accProps ls 
accProps ((False, _) : ls)      = accProps ls
            
-- |This function takes two arguments, a list of already translated arguments
-- (including the translated premises and exceptions) and a proposition 
-- paired with its to be translated arguments. It collects the results 
-- of the transApp function, which does the main work. 
transApps :: [LConcreteArg] -> (PropLiteral, [Argument]) -> ([LConcreteArg], [(LConcreteArg, LConcreteArg)])
transApps tArgs (p, args) = 
 let tr = map (transApp tArgs p) args
 in (map fst tr, concatMap snd tr)

-- |Given a list of already translated arguments and a propositional literal, 
-- an argument (pro the propositional literal) is translated into a Dung argument
-- and a possibly empty list of attackers. 
transApp :: [LConcreteArg] -> PropLiteral -> Argument -> (LConcreteArg, [(LConcreteArg, LConcreteArg)])
transApp tArgs _p a@(Arg (prems, excs, _c))
 | accProps tArgs `intersect` prems /= prems = ((False, Right a), [(defeater, (False, Right a))])
 | otherwise = 
 let acceptableExceptions = filter (\ (b, arg) -> b && either (`elem` excs) (const False) arg) tArgs
     applicableArg        = (null acceptableExceptions, Right a)
     defeats              = map (\ argExc -> (argExc, applicableArg)) acceptableExceptions
 in (applicableArg, defeats)

-- |Determines the maximum weight of a list of applicable arguments (assumed
-- to have the same conclusion).
maxWeight :: [LConcreteArg] -> CAES -> Double
maxWeight args (CAES (_, (_, argWeight), _))
 = foldl max 0 [argWeight a | (True, Right a) <- args]

 
-- |This function expects the following arguments: a propositional literal at
-- question, a list of pro arguments (labelled 'True', and thus acceptable in
-- the current AF), a list of con arguments (acceptable in the current AF) and 
-- a CAES. The result will be an argument corresponding to the proposition and 
-- a list of attacks. 
transAcc :: PropLiteral -> [LConcreteArg] -> [LConcreteArg] -> CAES -> (LConcreteArg, [(LConcreteArg, LConcreteArg)])
transAcc c [] _conArgs _caes = ((False, Left c),  [(defeater, (False, Left c))]) -- no applicable argument for p
transAcc _c ((_, Left _): _proArgs) _conArgs _caes = error "Proposition in the list of applicable arguments"
transAcc c ((False, _) : proArgs) conArgs caes = transAcc c proArgs conArgs caes
transAcc c proArgs@((True, _) : _) conArgs caes@(CAES (_, _, standard))
 | standard c == Scintilla  = ((True, Left c), []) -- there is an applicable argument for p, thus acceptable under Scintilla
 | standard c == Preponderance &&
   maxWeight proArgs caes > maxWeight conArgs caes = ((True, Left c), [])
 | standard c == ClearAndConvincing &&
   maxWeight proArgs caes > alpha &&
   maxWeight proArgs caes > maxWeight conArgs caes + beta = ((True, Left c), [])
 | standard c == BeyondReasonableDoubt &&
   maxWeight proArgs caes > alpha &&
   maxWeight proArgs caes > maxWeight conArgs caes + beta &&
   maxWeight conArgs caes < gamma = ((True, Left c), [])
 | standard c == DialecticalValidity && null conArgs  = ((True, Left c), [])
 | otherwise = ((False, Left c), [(defeater, (False, Left c))])

-- |Correspondence of the applicability of arguments. 
corApp :: CAES -> Bool
corApp caes@(CAES (argSet, _, _)) =
  let translatedCAES = translate caes
      applicableArgs  = filter (`applicable` caes)
                               (getAllArgs argSet)
      transArgs       = rights $ groundedExt translatedCAES
  in fromList applicableArgs == fromList transArgs

-- |Correspondence of the acceptability of propositional literals, including 
-- assumptions.
corAcc :: CAES -> Bool
corAcc caes@(CAES (argSet, (assumptions, _), _)) =
  let translatedCAES = translate caes
      acceptableProps = filter (\p -> p `acceptable` caes ||
                                      p `elem` assumptions)
                               (getProps argSet)
      transProps      = lefts $ delete (Left $ mkProp "defeater")
                                       (groundedExt translatedCAES)
  in fromList acceptableProps == fromList transProps