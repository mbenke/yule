module Core where
import Pretty
import Yul

-- type Name = String

data Type 
    = TInt 
    | TBool 
    | TPair Type Type 
    | TFun [Type] Type
    deriving (Show)

data Expr 
    = EInt Int 
    | EBool Bool 
    | EVar Name
    | EPair Expr Expr
    | EFst Expr
    | ESnd Expr 
    | ECall Name [Expr]

instance Show Expr where
    show = render . pretty

pattern SAV :: Name -> Expr -> Stmt
pattern SAV x e = SAssign (EVar x) e
data Stmt 
    = SAssign Expr Expr
    | SAlloc Name Type
    | SExpr Expr
    | SAssembly [YulStatement]
    | SReturn Name
    deriving Show

instance Pretty Expr where
    pretty (EInt i) = text (show i)
    pretty (EBool b) = text (show b)
    pretty (EVar x) = text x
    pretty (EPair e1 e2) = parens (pretty e1 >< comma <+> pretty e2)
    pretty (EFst e) = text "fst" <+> pretty e
    pretty (ESnd e) = text "snd" <+> pretty e
    pretty (ECall f es) = text f <+> parens(hsep (map pretty es))