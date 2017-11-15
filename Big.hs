-- Generates large Fortran files with lots of affine statements

module Big where

import Language.Fortran.AST as F
import Language.Fortran.ParserMonad as FP
import Language.Fortran.Util.Position as P

programStub n = F.ProgramFile meta [ unit ]
  where
    s        = P.SrcSpan P.initPosition P.initPosition
    meta     = F.MetaInfo (FP.Fortran90) filename
    filename = name ++ ".f90"
    name     = "big" ++ n
    unit     =
      F.PUMain () s (Just name) blocks Nothing
    blocks   = []
