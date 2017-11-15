-- Generates large Fortran files with lots of affine statements

module Main where

import Language.Fortran.AST as F
import Language.Fortran.ParserMonad as FP
import Language.Fortran.Util.Position as P
import Language.Fortran.PrettyPrint as FPP
import Text.PrettyPrint (render)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if length args < 4
    then putStrLn "Usage: big <fileName> <numOfFunctions> <lengthOfFunction> <numOfFunArgs>"
    else do
      let (name : funs : funLength : funArgs : _) = args
      if (read funArgs :: Int) > read funLength
        then putStrLn "Number of function args should be less than the function length."
        else do
         let p = programStub  name (read funs) (read funLength) (read funArgs)
         let sourceText = pprint FP.Fortran90 p Nothing
         writeFile (name ++ ".f90") (render sourceText)

{- | `programStub name f l a` generates a Fortran AST named `name`
      with `f` functions with `l` assignments in each and `a` arguments in each -}

programStub :: String -> Int -> Int -> Int -> F.ProgramFile ()
programStub name funs funLength funArgs =
    F.ProgramFile meta units
  where
    meta     = F.MetaInfo FP.Fortran90 filename
    filename = name ++ ".f90"
    units = map (function funArgs) (partition [1..funs*funLength] funs)
      -- F.PUMain () nullSpan (Just name) (decls ++ blocks) Nothing

function :: Int -> [Int] -> F.ProgramUnit ()
function numArgs ids =
    F.PUFunction () nullSpan (Just realType) opts name args Nothing body Nothing
  where
    name = "f" ++ concatMap show ids
    opts = F.None () nullSpan False
    -- Arugments of the function
    args = Just (F.fromList () (map variable (take numArgs ids)))
    -- Create the entire body of the function
    body = decls ++ blocks ++ [returnStmt]
    -- Type declarations
    decls = map declaration ids
    -- Statements for the body
    blocks = map block (drop numArgs ids)
    -- Return statement
    returnStmt = F.BlStatement () nullSpan Nothing
      $ F.StExpressionAssign () nullSpan
          (F.ExpValue () nullSpan $ F.ValVariable name)
          (variable (last ids))

{- | `block n` generate a statement (block) for variable `n` -}
block :: Int -> F.Block ()
block n =
     F.BlStatement () nullSpan Nothing stmt
  where
     stmt =  F.StExpressionAssign () nullSpan left right
     left =  variable n
     right = F.ExpBinary () nullSpan F.Multiplication e1 e2
     e1 = variable (n - 1)
     e2 = variable (n - 2)

variable :: Int -> F.Expression ()
variable x = F.ExpValue () nullSpan (F.ValVariable $ "v" ++ show x)

declaration :: Int -> F.Block ()
declaration n =
     F.BlStatement () nullSpan Nothing stmt
   where
     stmt = F.StDeclaration () nullSpan realType Nothing (F.fromList () [decl])
     decl = F.DeclVariable () nullSpan expr Nothing initVal
     expr = F.ExpValue () nullSpan varValue
     varValue = F.ValVariable $ "v" ++ show n
     initVal = Nothing

realType :: F.TypeSpec ()
realType = F.TypeSpec () nullSpan F.TypeReal Nothing

nullSpan :: SrcSpan
nullSpan = P.SrcSpan P.initPosition P.initPosition

partition :: [a] -> Int -> [[a]]
partition [] _ = []
partition xs n =
  take n xs : partition (drop n xs) n
