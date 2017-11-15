-- Generates large Fortran files with lots of affine statements

module Main where

import Language.Fortran.AST as F
import Language.Fortran.ParserMonad as FP
import Language.Fortran.Util.Position as P
import Language.Fortran.PrettyPrint as FPP
import Text.PrettyPrint (render)

main = do
  let p = programStub 0
  let sourceText = pprint FP.Fortran90 p Nothing
  writeFile "big0.f90" (render sourceText)

programStub n = F.ProgramFile meta [ unit ]
  where
    s        = P.SrcSpan P.initPosition P.initPosition
    meta     = F.MetaInfo (FP.Fortran90) filename
    filename = name ++ ".f90"
    name     = "big" ++ show n
    unit     =
      F.PUMain () s (Just name) blocks Nothing
    blocks   = []
