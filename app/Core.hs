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
    | SComment String
    -- deriving Show
instance Show Stmt where show = render . pretty

data Core = Core [Stmt]

instance Pretty Type where
    pretty TInt = text "int"
    pretty TBool = text "bool"
    pretty (TPair t1 t2) = parens (pretty t1 <+> text "*" <+> pretty t2)
    pretty (TFun ts t) = parens (hsep (map pretty ts) <+> text "->" <+> pretty t)
    
instance Pretty Expr where
    pretty (EInt i) = text (show i)
    pretty (EBool b) = text (show b)
    pretty (EVar x) = text x
    pretty (EPair e1 e2) = parens (pretty e1 >< comma <+> pretty e2)
    pretty (EFst e) = pretty e  >< text ".fst"
    pretty (ESnd e) = pretty e  >< text ".snd"
    pretty (ECall f es) = text f >< parens(hsep (map pretty es))

instance Pretty Stmt where
    pretty (SAssign lhs rhs) = pretty lhs <+> text ":=" <+> pretty rhs
    pretty (SAlloc x t) = text "let" <+> text x <+> text ":" <+> pretty t
    pretty (SExpr e) = pretty e
    pretty (SAssembly stmts) = vcat (map pretty stmts)
    pretty (SReturn x) = text "return" <+> text x
    pretty (SComment c) = text "//" <+> text c

instance Pretty Core where
    pretty (Core stmts) = vcat (map pretty stmts)