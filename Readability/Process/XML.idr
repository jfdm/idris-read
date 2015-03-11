module Readability.Process.XML

import Prelude.Strings

import Effects
import Effect.State

import XML.DOM
import XML.XPath

import Readability.WordTypes
import Readability.Stats
import Readability.Metrics

import Readability.Process.Common

||| Extract the sentences from a text node.
getSentences : Document a -> List String
getSentences (Text t) = Strings.split (isEOS) t
  where
    isEOS : Char -> Bool
    isEOS e = List.elem e ['.', ':', '!', '"', '\'']

processPara : List String -> {[STATE RStats]} Eff ()
processPara Nil     = pure ()
processPara (s::ss) = do
  processSentence $ words s
  update (\x => record {sentances = (sentances x) + 1} x)
  processPara ss

processParas : List $ Document NODE -> {[STATE RStats]} Eff ()
processParas Nil     = pure ()
processParas (Node p::ps) = do
   processPara (getSentences p)
   processParas ps

calcReadability : Document DOCUMENT
                -> {[STATE RStats]} Eff $ List (RMetricTy, Float)
calcReadability doc = case queryDoc "//*/text()" doc of
      Left err => pure Nil
      Right ps => do
        processParas ps
        res <- get
        pure $ calcScores res


-- --------------------------------------------------------------------- [ EOF ]
