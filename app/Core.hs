module Core where
import Pretty
import Yul

-- type Name = String

data Type
    = TInt
    | TBool
    | TPair Type Type
    | TSum Type Type
    | TFun [Type] Type
    | TUnit
    deriving (Show)

data Expr
    = EInt Int
    | EBool Bool
    | EVar Name
    | EPair Expr Expr
    | EFst Expr
    | ESnd Expr
    | EInl Expr
    | EInr Expr
    | ECall Name [Expr]
    | EUnit
instance Show Expr where
    show = render . pretty

pattern SAV :: Name -> Expr -> Stmt
pattern SAV x e = SAssign (EVar x) e
data Stmt
    = SAssign Expr Expr
    | SAlloc Name Type
    | SExpr Expr
    | SAssembly [YulStatement]
    | SReturn Expr
    | SComment String
    | SBlock [Stmt]
    | SCase Expr [Alt]
    -- deriving Show
instance Show Stmt where show = render . pretty

data Alt = Alt Name Stmt

data Core = Core [Stmt]

instance Pretty Type where
    pretty TInt = text "int"
    pretty TBool = text "bool"
    pretty TUnit = text "unit"
    pretty (TPair t1 t2) = parens (pretty t1 <+> text "*" <+> pretty t2)
    pretty (TSum t1 t2) = parens (pretty t1 <+> text "+" <+> pretty t2)
    pretty (TFun ts t) = parens (hsep (map pretty ts) <+> text "->" <+> pretty t)

instance Pretty Expr where
    pretty (EInt i) = text (show i)
    pretty (EBool b) = text (show b)
    pretty EUnit = text "()"
    pretty (EVar x) = text x
    pretty (EPair e1 e2) = parens (pretty e1 >< comma <+> pretty e2)
    pretty (EFst e) = pretty e  >< text ".fst"
    pretty (ESnd e) = pretty e  >< text ".snd"
    pretty (EInl e) = text "inl" >< parens (pretty e)
    pretty (EInr e) = text "inr" >< parens (pretty e)
    pretty (ECall f es) = text f >< parens(hsep (map pretty es))

instance Pretty Stmt where
    pretty (SAssign lhs rhs) = pretty lhs <+> text ":=" <+> pretty rhs
    pretty (SAlloc x t) = text "let" <+> text x <+> text ":" <+> pretty t
    pretty (SExpr e) = pretty e
    pretty (SAssembly stmts) = vcat (map pretty stmts)
    pretty (SReturn e) = text "return" <+> pretty e
    pretty (SComment c) = text "//" <+> text c
    pretty (SBlock stmts) = lbrace $$ nest 4 (vcat (map pretty stmts)) $$ rbrace
    pretty (SCase e alts) =
        text "match" <+> pretty e <+> text "with" $$ lbrace
           $$ nest 2 (vcat (map pretty alts))  $$ rbrace

instance Pretty Alt where
    pretty (Alt n s) = text n <+> text "=>" <+> pretty s

instance Pretty Core where
    pretty (Core stmts) = vcat (map pretty stmts)
