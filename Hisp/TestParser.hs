import Hisp.Parser
import Hisp.Base

import Text.Parsec.Prim

---

test = parseExp initialState

test2 p = runParser p initialState "(s-exp)"
