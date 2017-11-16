-- Generates large Fortran files with lots of affine statements

module Big.Big where

import Language.Fortran.AST as F
import Language.Fortran.ParserMonad as FP
import Language.Fortran.Util.Position as P
import Language.Fortran.PrettyPrint as FPP
import Text.PrettyPrint (render)
import System.Environment (getArgs)

-- Compile with:
--  ghc Big.hs -o big

-- Example usage:
--  ./big separate big 5 5 1
-- Generates big.f90 as the top-level and big1.f90..big5.f90 which each
-- contain a function in a module, of length 5, with one argument each

main :: IO ()
main = do
  args <- getArgs
  if length args < 5
    then putStrLn "Usage: big separate|whole <name> <numOfFunctions> <lengthOfFunction> <numOfFunArgs>"
    else do
      let (mode : name : funs : funLength : funArgs : _) = args
      bigSynthesise mode name (read funs) (read funLength) (read funArgs)

bigSynthesise :: String -> String -> Int -> Int -> Int -> IO ()
bigSynthesise mode name funs funLength funArgs
 | funLength < 2 = putStrLn "Length of a function should be at least 2"
 | funArgs > funLength = putStrLn "Number of function args should be less than the function length."
 | otherwise = mapM_ outputFile (programs mode name funs funLength funArgs)
    where
      outputFile (name', p) = do
        let sourceText = pprint FP.Fortran90 p Nothing
        writeFile (name' ++ ".f90") (render sourceText)


{- | `programStub mode name f l a` generates a Fortran AST named `name`
      with `f` functions with `l` assignments in each and `a` arguments in each -}

programs :: String -> String -> Int -> Int -> Int -> [(String, F.ProgramFile ())]
-- Generate a single program
programs mode name funs funLength funArgs =
    case mode of
       "whole" ->
             [(name, topLevel (mainP `contains` Just units))]
       "separate" ->
         let uses = map (mkUse name) [1..funs]
         in   (name, topLevel ((uses ++ mainP) `contains` Nothing))
           : zipWith (mkModule name) [1..funs] units

       _ -> error "Mode not supported, use either: whole or separate"
  where
    units       = zipWith (function funArgs) [1..funs] (partition [1..funs*funLength] funLength)
    topLevel   u = F.ProgramFile meta [u]
    meta        = F.MetaInfo FP.Fortran90 (name ++ ".f90")
    contains    = F.PUMain () nullSpan (Just name)
    mainP       = mainProgram funs funArgs

mkModule :: String -> Int -> F.ProgramUnit () -> (String, F.ProgramFile ())
mkModule name n unit
  = ("mod_" ++ name', F.ProgramFile meta' [pmod])
     where
      pmod  = F.PUModule () nullSpan name' [] (Just [unit])
      name' = name ++ show n
      meta'  = F.MetaInfo FP.Fortran90 (name' ++ ".f90")

mkUse :: String -> Int -> F.Block ()
mkUse name n =
  F.BlStatement () nullSpan Nothing
    $ F.StUse () nullSpan (variableStr (name ++ show n)) F.Permissive Nothing

{- | `mainProgram n k` generates a sequence of blocks (for a program) which
      comprise function calls to functions f1..fn of k-arity with a shared
      result variable but with separate argument variables -}
mainProgram :: Int -> Int -> [F.Block ()]
mainProgram funs funArgs = typeDecls ++ calls
   where
     typeDecls = map declaration [0..funs*funArgs]
     calls     = zipWith mkCalls [1..funs] (partition [1..funs*funArgs] funArgs)
     mkCalls n args =
       F.BlStatement () nullSpan Nothing
         $ F.StExpressionAssign () nullSpan (variable 0)
          $ F.ExpFunctionCall () nullSpan (variableStr $ "f" ++ show n)
           $ Just (F.fromList () (map (toArgument . variable) args))
     toArgument = F.Argument () nullSpan Nothing

function :: Int -> Int -> [Int] -> F.ProgramUnit ()
function  numArgs funNumber ids =
    F.PUFunction () nullSpan (Just realType) opts name args Nothing body Nothing
  where
    name = "f" ++ show funNumber
    opts = F.None () nullSpan False
    -- Arugments of the function
    args = Just (F.fromList () (map variable (take numArgs ids)))
    -- Create the entire body of the function
    body = decls ++ blocks ++ [returnStmt]
    -- Type declarations
    decls = map declaration ids
    -- Statements for the body
    blocks = map block (drop 2 ids)
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
