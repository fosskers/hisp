module Hisp.Flags where

import System.Console.GetOpt

---

data Flag = Load | Exec deriving (Eq,Show)

flags :: [OptDescr Flag]
flags = [ Option ['l'] ["load"] (NoArg Load) "Load a .hisp file into the REPL."
        , Option ['e'] ["exec"] (NoArg Exec) "Execute given Hisp code." ]

parseFlags :: [String] -> ([Flag],[String])
parseFlags args = case getOpt Permute flags args of
                         (opts,nonOpts,_) -> (opts,nonOpts)
