module Main where
import Prelude hiding ((<>))

import Core
import CoreParser
import Pretty
import TM
import Translate
import Yul
import YulParser

main :: IO ()
main = main9

main9 :: IO ()
main9 = do
    src <- readFile "examples/01pairs.core"
    let core = parseCore src
    putStrLn "/* Core:"
    putStrLn (render (nest 2 (pretty core)))
    putStrLn "*/"
    generatedYul <- runTM (translateCore core)
    let fooFun = wrapInSolFunction "main" generatedYul
    let doc = wrapInContract "Foo" "main()" fooFun
    putStrLn (render doc)

main3 :: IO ()
main3 = do
    generatedYul <- runTM (translateCore core2)
    let fooFun = wrapInSolFunction "main" generatedYul
    let doc = wrapInContract "Foo" "main()" fooFun
    putStrLn (render doc)

main5 :: IO ()
main5 = putStrLn (render (pretty core2))

{-
main6 = do
    putStrLn "/* Core:"
    putStrLn (render (nest 2 (pretty core2)))
    putStrLn "*/"
    main3
-}
main8 :: IO ()
main8 = do
    src <- readFile "foo.yul"
    let yul = parseYul src
    putStrLn (render (pretty yul))

main7 :: IO ()
main7 = do
    putStrLn "/* Core:"
    putStrLn (render (nest 2 (pretty core3)))
    putStrLn "*/"
    generatedYul <- runTM (translateCore core3)
    let fooFun = wrapInSolFunction "main" generatedYul
    let doc = wrapInContract "Foo" "main()" fooFun
    putStrLn (render doc)

core3 :: Core
core3 = Core
  [ SAlloc "s" (TSum TBool TInt)
  , SAssign s (EInl (EBool False))
  , SAssign s (EInr(EInt 42))
  , SReturn (EInt 0)
  , SCase s [Alt "b" (SReturn (EInt 17))
            ,Alt "i" (SReturn (EVar "i"))]
  ] where s = EVar "s"

core2 :: Core
core2 = Core [
    SAlloc a TInt,
    SAlloc c TInt,
    SAlloc b (TPair TInt TInt),
    SAV a (EInt 42),
    SAV b (EPair va (EInt 0)),
    SAssign (EFst vb) (EInt 1337),
    SAV c (EFst vb),
    SReturn vc
    ] where
        (a,b,c) = ("a","b","c")
        (va, vb, vc) = (EVar a, EVar b, EVar c)


{-

core1 :: Stmt
core1 = SAssembly [yfun1, YulAssign ["_result" ] (YulCall "f" [])]

yul2 :: Yul
yul2 = parseYul
        "let a, b := 1 function foo(x) { let y := x }"

yul1 :: Yul
yul1 = Yul [yfun1, YulAssign ["_result" ] (YulCall "f" [])]

yfun1 :: YulStatement
yfun1 = YulFun "f" [] (YReturns["_funresult"])
    [ YulAlloc "x"
    , YulAssign ["x"] (YulLiteral (YulNumber 42))
    , YulAssign ["_funresult"] (YulIdentifier "x")
    ]

main1 = putStrLn (render (pretty yul1))
main2 = putStrLn (render (pretty yul2))
-}
