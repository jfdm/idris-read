-- ----------------------------------------------------------------- [ XML.idr ]
-- Module    : XML.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Readability.Process.XML

import Prelude.Strings

import Effects
import Effect.State

import XML.DOM
import XML.XPath

import Readability.WordTypes
import Readability.Stats
import Readability.Metrics

import Readability.Process.Effs
import public Readability.Process.Common

||| Extract the sentences from a text node.
getSentences : Document a -> List String
getSentences (Text t) = Strings.split (isEOS) t
  where
    isEOS : Char -> Bool
    isEOS e = List.elem e ['.', ':', '!', '"', '\'']

processPara : List String -> Eff () ReadEffs
processPara Nil     = pure ()
processPara (s::ss) = do
  processSentence $ words s
  updateReadState (\x => record {sentances = (sentances x) + 1} x)
  processPara ss

processParas : List $ Document NODE -> Eff () ReadEffs
processParas Nil     = pure ()
processParas (Node p::ps) = do
   processPara (getSentences p)
   processParas ps


export
calcReadabilityE : Document DOCUMENT
                -> Eff (Maybe ReadResult) ReadEffs
calcReadabilityE doc =
  case query "//*/text()" doc of
      Left err => pure Nothing
      Right ps => do
        processParas ps
        res <- getReadState
        pure $ Just (calcScores res)

export
calcReadability : Document DOCUMENT -> Maybe ReadResult
calcReadability doc = runPure $ calcReadabilityE doc

-- --------------------------------------------------------------------- [ EOF ]
