-- | This is the examples module accompanying the implementation of the 
-- translation from the Carneades argumentation model into Dung's 
-- argumentation frameworks. 
--
-- This module contains a collection of examples, showing how to translate
-- an existing Carneades Argument Evaluation Structure (CAES) into an argumentation
-- framework. 
--
-- To run these examples, or your own: start GHCi and do the following:
--
-- @\:l Language.CarneadesIntoDung.Examples@
-- 

module Language.CarneadesIntoDung.Examples 
  (
   -- *Example translation
   -- |We use the example CAES as defined in "Language.Carneades.ExampleCAES".
   caes, 
   exTrans, exTrans',
   -- *Correspondence properties
   corProp,    
  )
 where
import Language.CarneadesIntoDung.Translation
import Language.Carneades.CarneadesDSL (CAES)
import Language.Carneades.ExampleCAES

-- |Translation of the example CAES 'caes'.
-- The following is the prettified output of the translation, where
-- the five propositions in the middle are the assumptions and 'defeater'.
-- 
-- >>> translate caes
-- AF [
-- Left (True,"murder"),
-- Left (False,"murder"),
-- Right ["kill","intent"] ~[]=>"murder",
-- Left (False,"intent"),
-- Left (True,"intent"),
-- Right ["witness2"] ~["unreliable2"]=>"-intent",
-- Right ["witness"] ~["unreliable"]=>"intent",
-- Left (True,"unreliable"),
-- Left (False,"unreliable")
-- ,
-- Left (True,"defeater"),
-- Left (True,"kill"),
-- Left (True,"witness"),
-- Left (True,"witness2"),
-- Left (True,"unreliable2")] 
-- [
-- (Left (True,"defeater"), Left (True,"murder")),
-- (Left (True,"defeater"), Left (False,"murder")),
-- (Left (True,"defeater"), Right ["kill","intent"] ~[]=>"murder"),
-- (Left (True,"defeater"), Left (False,"intent")),
-- (Left (True,"defeater"),Left (True,"intent")),
-- (Left (True,"unreliable2"),Right ["witness2"] ~["unreliable2"]=>"-intent"),
-- (Left (True,"defeater"),Left (True,"unreliable")),
-- (Left (True,"defeater"),Left (False,"unreliable"))
-- ]
exTrans :: ConcreteAF
exTrans = translate caes



-- |Translation of the example CAES 'caes', keeping labels.
-- The following is the prettified output of the translation, where 
-- the five propositions in the middle are the assumptions and 'defeater'.
--
-- >>> translate' caes
-- AF [
-- (False,Left (True,"murder")),
-- (False,Left (False,"murder")),
-- (False,Right ["kill","intent"] ~[]=>"murder"),
-- (False,Left (False,"intent")),
-- (False,Left (True,"intent")),
-- (False,Right ["witness2"] ~["unreliable2"]=>"-intent"),
-- (True,Right ["witness"] ~["unreliable"]=>"intent"),
-- (False,Left (True,"unreliable")),
-- (False,Left (False,"unreliable"))
-- ,
-- (True,Left (True,"defeater")),
-- (True,Left (True,"kill")),
-- (True,Left (True,"witness")),
-- (True,Left (True,"witness2")),
-- (True,Left (True,"unreliable2"))]
-- [
-- ((True,Left (True,"defeater")),(False,Left (True,"murder"))),
-- ((True,Left (True,"defeater")),(False,Left (False,"murder"))),
-- ((True,Left (True,"defeater")), (False,Right ["kill","intent"] ~[]=>"murder")),
-- ((True,Left (True,"defeater")),(False,Left (False,"intent"))),
-- ((True,Left (True,"defeater")),(False,Left (True,"intent"))),
-- ((True,Left (True,"unreliable2")),(False,Right ["witness2"] ~["unreliable2"]=>"-intent")),
-- ((True,Left (True,"defeater")),(False,Left (True,"unreliable"))),
-- ((True,Left (True,"defeater")),(False,Left (False,"unreliable")))
exTrans' :: LConcreteAF
exTrans' = translate' caes



-- |Combining the correspondence properties.
--
-- And as expected:
-- 
-- >>> corApp caes && corAcc caes
-- True
corProp :: CAES -> Bool
corProp caes = corApp caes && corAcc caes