-- Generates large Fortran files with lots of affine statements

module Main where

import Language.Fortran.AST as F
import Language.Fortran.ParserMonad as FP
import Language.Fortran.Util.Position as P
import Language.Fortran.PrettyPrint as FPP
import Text.PrettyPrint (render)

main :: IO ()
main = do
  let name = "big0"
  let p = programStub 10 4 name
  let sourceText = pprint FP.Fortran90 p Nothing
  writeFile (name ++ ".f90") (render sourceText)

{- | `programStub n m name` generates a Fortran AST named `name`
      with n*m assignments of the form x = a * b with dependency
      chains of length m -}

programStub :: Int -> Int -> String -> F.ProgramFile ()
programStub depth width name = F.ProgramFile meta [ unit ]
  where
    meta     = F.MetaInfo FP.Fortran90 filename
    filename = name ++ ".f90"
    unit     =
      F.PUMain () nullSpan (Just name) (decls ++ blocks) Nothing
    -- Type declarations
    decls = map mkDeclaration [1..depth*width]
    -- Statement blocks
    blocks  = map (block width) [1..depth*width]

-- Generate a list of blocks of length n
block :: Int -> Int -> F.Block ()
block width n =
     F.BlStatement () nullSpan Nothing stmt
  where
     stmt = F.StExpressionAssign () nullSpan left right
     left =  F.ExpValue () nullSpan (F.ValVariable $ "v" ++ show n)
     right =
      if n `mod` width == 0 || n `mod` width == 1
        then
          F.ExpValue () nullSpan (F.ValInteger $ show n)
        else
          F.ExpBinary () nullSpan F.Multiplication e1 e2
            where
              e1 = F.ExpValue () nullSpan (F.ValVariable $ "v" ++ show (n - 1))
              e2 = F.ExpValue () nullSpan (F.ValVariable $ "v" ++ show (n - 2))

mkDeclaration :: Int -> F.Block ()
mkDeclaration n =
     F.BlStatement () nullSpan Nothing stmt
   where
     stmt = F.StDeclaration () nullSpan typ Nothing (F.fromList () [decl])
     decl = F.DeclVariable () nullSpan expr Nothing initVal
     expr = F.ExpValue () nullSpan varValue
     varValue = F.ValVariable $ "v" ++ show n
     initVal = Nothing
     typ  = F.TypeSpec () nullSpan F.TypeReal Nothing

{-
freshVar :: State Int String
freshVar = do
  x <- get
  put (x+1)
  return $ "v" ++ show x
-}

nullSpan :: SrcSpan
nullSpan = P.SrcSpan P.initPosition P.initPosition
