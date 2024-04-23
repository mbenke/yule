module Main where
import Prelude hiding ((<>))

import Core
import Pretty
import TM
import Translate
import Yul
-- import YulParser

main :: IO ()
main = main3
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

main7 :: IO ()
main7 = do
    putStrLn "/* Core:"
    putStrLn (render (nest 2 (pretty core3)))
    putStrLn "*/"

core3 :: Core
core3 = Core
  [ SAlloc "s" TInt
  , SAssign s (EInl (EBool False))
  , SAssign s (EInr (EInt 42))
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
