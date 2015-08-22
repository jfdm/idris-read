-- ------------------------------------------------------- [ Effs.idr<Process> ]
-- Module    : Effs.idr<Process>
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Readability.Process.Effs

import Effects
import Effect.State

import Readability.Stats

ReadEffs : List EFFECT
ReadEffs = ['readST ::: STATE RStats]

getReadState : Eff RStats ['readST ::: STATE RStats]
getReadState = 'readST :- get

putReadState : RStats -> Eff () ['readST ::: STATE RStats]
putReadState r = 'readST :- put r

updateReadState : (RStats -> RStats) -> Eff () ['readST ::: STATE RStats]
updateReadState f = 'readST :- update f

-- --------------------------------------------------------------------- [ EOF ]
