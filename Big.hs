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
  if length args < 5
    then putStrLn "Usage: big Separate|Whole <name> <numOfFunctions> <lengthOfFunction> <numOfFunArgs>"
    else do
      let (mode : name : funs : funLength : funArgs : _) = args
      if (read funArgs :: Int) > read funLength
        then putStrLn "Number of function args should be less than the function length."
        else do
         let ps = programStub (read mode) name (read funs) (read funLength) (read funArgs)
         mapM_ outputFile ps
  where
    outputFile (name, p) = do
      let sourceText = pprint FP.Fortran90 p Nothing
      writeFile (name ++ ".f90") (render sourceText)

data Mode = Separate | Whole deriving Read

{- | `programStub mode name f l a` generates a Fortran AST named `name`
      with `f` functions with `l` assignments in each and `a` arguments in each -}

programStub :: Mode -> String -> Int -> Int -> Int -> [(String, F.ProgramFile ())]
-- Generate a single program
programStub Whole name funs funLength funArgs =
    [(name, F.ProgramFile meta [unit])]
  where
    unit  = F.PUMain () nullSpan (Just name) [] (Just units)
    units = map (function funArgs) (partition [1..funs*funLength] funs)
    meta  = F.MetaInfo FP.Fortran90 (name ++ ".f90")
-- Generate separate modules
programStub Separate name funs funLength funArgs =
    topLevel : zipWith mkModule [1..funs] units
  where
    topLevel = (name, F.ProgramFile meta [topUnit])
    meta  = F.MetaInfo FP.Fortran90 (name ++ ".f90")

    topUnit = F.PUMain () nullSpan (Just name) uses Nothing
    uses = map mkUse [1..funs]

    mkUse n =
      F.BlStatement () nullSpan Nothing
        $ F.StUse () nullSpan (variableStr (name ++ show n)) F.Permissive Nothing

    units = map (function funArgs) (partition [1..funs*funLength] funs)
    mkModule n unit
      = (name', F.ProgramFile meta' [pmod])
         where
          pmod  = F.PUModule () nullSpan name' [] (Just [unit])
          name' = name ++ show n
          meta'  = F.MetaInfo FP.Fortran90 (name' ++ ".f90")



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
      $ F.StExpressionAssign () nullSpan (variableStr name) (variable (last ids))

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
variable x = variableStr $ "v" ++ show x

variableStr :: String -> F.Expression ()
variableStr x = F.ExpValue () nullSpan (F.ValVariable x)

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
