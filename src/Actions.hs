module Actions
(
    alterDoor,
    wireCmdBlock
) where

import Datatypes

alterDoor :: Action
alterDoor s = case floSpot s of
    Door _ False -> s { floSpot = ((floSpot s) { symFlo = '/', isOpen = True }) }
    Door _ True  -> s { floSpot = ((floSpot s) { symFlo = '+', isOpen = False }) }
    _            -> s

wireCmdBlock :: Action -> Coords -> Action
wireCmdBlock nact nloc s = case floSpot s of
    CmdBlock _ act loc -> s { floSpot = ((floSpot s) { act = nact, loc = nloc }) }
    _                  -> s
