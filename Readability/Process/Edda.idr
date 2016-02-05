-- ---------------------------------------------------------------- [ Edda.idr ]
-- Module    : Edda.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Readability.Process.Edda

import Effects
import Effect.State

import Edda
import Edda.Query

import Readability.WordTypes
import Readability.Stats
import Readability.Metrics

import Readability.Process.Effs
import public Readability.Process.Common

-- ----------------------------------------------------------------- [ Queries ]
||| Extract text from inlined elements.
extractText : Edda PRIME INLINE -> List String
extractText (Text t) = [t]
extractText _        = Nil

||| Extract paragraphs from documents.
extractPara : Edda PRIME BLOCK -> List (Edda PRIME BLOCK)
extractPara (Para txt) = [Para txt]
extractPara _          = Nil

||| Do extraction of paragraphs from a document.
getParas : Edda PRIME MODEL -> List (Edda PRIME BLOCK)
getParas = query extractPara

-- --------------------------------------------------------- [ Stat Collection ]

||| Extract Sentences from a paragraph.
getSentances : (Edda PRIME BLOCK) -> List (List (Edda PRIME INLINE))
getSentances (Para txt) = doSplit txt
  where
    isEOS : (Edda PRIME INLINE) -> Bool
    isEOS e = List.elem e [Period, Colon, Bang, QMark, SMark]

    doSplit : List (Edda PRIME INLINE) -> List (List (Edda PRIME INLINE))
    doSplit xs = filter (\x => x /= Nil) (List.split (isEOS) xs)

||| Process a paragraph, which is a list of sentences.
processPara : List (List (Edda PRIME INLINE)) -> Eff () ReadEffs
processPara Nil     = pure ()
processPara (s::ss) = do
  processSentence (query extractText s)
  updateReadState (\x => record {sentances = (sentances x) + 1} x)
  processPara ss

||| Process paragraphs.
processParas : List (Edda PRIME BLOCK) -> Eff () ReadEffs
processParas Nil      = pure ()
processParas (p::ps) = do
  processPara (getSentances p)
  processParas ps

export
calcReadabilityE : Edda PRIME MODEL
                -> Eff (Maybe ReadResult) ReadEffs
calcReadabilityE doc = do
    case getParas doc of
      Nil => pure Nothing
      ps  => do
        processParas ps
        res <- getReadState
        pure $ Just (calcScores res)

export
calcReadability : Edda PRIME MODEL -> Maybe ReadResult
calcReadability doc = runPure $ calcReadabilityE doc

-- --------------------------------------------------------------------- [ EOF ]
