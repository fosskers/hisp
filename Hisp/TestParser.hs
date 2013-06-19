import Hisp.Parser
import Hisp.Base

import Text.Parsec.Prim

---

test p = runParser p [] "(Testing)"
