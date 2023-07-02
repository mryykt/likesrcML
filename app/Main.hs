{-# LANGUAGE LambdaCase #-}
module Main (main) where
import Language.Haskell.Exts
import Data.List.Utils
import System.Environment(getArgs)

tag::String->String->String
tag t b="<"++t++">"++b++"</"++t++">"

sname::Name SrcSpanInfo->String
sname=tag "name" . sub
  where
    sub (Ident _ str)=tag "ident" str
    sub (Symbol _ str)=tag "symbol" str

smname::ModuleName SrcSpanInfo->String
smname (ModuleName _ s)=tag "module_name" s

sspcon::SpecialCon SrcSpanInfo->String
sspcon=tag "special_constructor" . sub
  where
    sub (UnitCon _)=tag "unit" "()"
    sub (ListCon _)=tag "list" "[]"
    sub (FunCon _)=tag "function" "->"
    sub (TupleCon _ b i)=tag "tuple" $ "("++take i (cycle ",")++")"
    sub (Cons _)=tag "cons" "(:)"
    sub (UnboxedSingleCon _)=tag "unboxed_single_tuple" "(# #)"
    sub (ExprHole _)=tag "exp_hole" "_"

sqname::QName SrcSpanInfo->String
sqname=tag "qname" . sub
  where
    sub (Qual _ mdl n)=tag "qualified" $ smname mdl++"."++sname n
    sub (UnQual _ n)=tag "unqualified" $ sname n
    sub (Special _ s)=tag "special" $ sspcon s

sqop::QOp SrcSpanInfo->String
sqop=tag "qop" . sub
  where
    sub (QVarOp _ qn)=tag "variable_op" (sqname qn)
    sub (QConOp _ qn)=tag "constructor_op" (sqname qn)

ssign::Sign SrcSpanInfo->String
ssign (Signless _)=""
ssign (Negative _)="<negative_sign/>-"

spat::Pat SrcSpanInfo->String
spat =tag "pat" . sub
  where
    sub (PVar _ n)=sname n
    sub (PLit _ s l)=ssign s++sliteral l
    sub (PNPlusK _ n i)=undefined -- n+k
    sub (PApp _ qn ps)=tag "data_constructor" (sqname qn++" "++join " " (map spat ps))
    sub (PTuple _ b ps)=tag "tuple" ("("++join "," (map sub ps)++")")
    sub (PParen _ p)="("++sub p++")"
    sub (PWildCard _)=tag "wild_card" "_"
    sub p=error (show p)

sliteral::Literal SrcSpanInfo->String
sliteral=tag "literal" . sub
  where
    sub (Char _ _ str)=tag "char" ("'"++str++"'")
    sub (String _ _ str)=tag "string" ("\""++str++"\"")
    sub (Int _ _ str)=tag "int" str
    sub (Frac _ _ str)=tag "floating_point" str
    sub (PrimInt _ _ str)=tag "unboxed_int" str
    sub (PrimWord _ _ str)=tag "unboxed_word" str
    sub (PrimFloat _ _ str)=tag "unboxed_float" str
    sub (PrimDouble _ _ str)=tag "unboxed_double" str
    sub (PrimChar _ _ str)=tag "unboxed_char" str
    sub (PrimString _ _ str)=tag "unboxed_string" str

srhs::Rhs SrcSpanInfo->String
srhs=tag "rhs" . sub
  where
    sub (UnGuardedRhs _ e)=tag "unguarded_rhs" (sexp e)
    sub _=undefined

sexp::Exp SrcSpanInfo->String
sexp=tag "exp" . sub
  where
    sub (Var _ qn)=tag "var" (sqname qn)
    sub (OverloadedLabel _ s)=tag "overloaded_label" s
    sub (IPVar _ ipn)=tag "ipvar" undefined
    sub (Con _ qn)=tag "data_constructor" (sqname qn)
    sub (Lit _ ltr)=tag "literal" (sliteral ltr)
    sub (InfixApp _ e1 qop e2)=tag "infix_app" (tag "left" (sub e1)++tag "op" (sqop qop)++tag "right" (sub e2))
    sub (App _ e1 e2)=tag "app" (tag "fun" (sub e1)++" "++tag "arg" (sub e2))
    sub (NegApp _ e)=tag "neg_app" ("-"++sub e)
    sub (Lambda _ ps e)=tag "lambda" $ "\\"++join " " (map spat ps)++"->"++sub e
    sub (Let _ bs e)=tag "let" ("let "++sbinds bs++"in "++sub e)
    sub (If _ cond thn els)=tag "if" (" "++tag "condition" (sub cond)++" then "++tag "then" (sub thn)++" else "++tag "else" (sub els))
    sub (MultiIf _ grhs)=tag "multi_if" undefined
    sub (Case _ e as)=tag "case" undefined
    sub (Do _ ss)=tag "do_exp" undefined
    sub (MDo _ ss)=tag "mdo_exp" undefined
    sub (Tuple _ b es)=tag "tuple" undefined
    sub (UnboxedSum _ i1 i2 e)=tag "unboxed_sum" undefined
    sub (TupleSection _ b es)=tag "tuple_section" undefined
    sub (List _ es)=tag "list" ("["++join "," (map sub es)++"]")
    sub (ParArray _ es)=tag "paralell_array" undefined
    sub (Paren _ e)="("++sub e++")"
    sub (LeftSection _ e qop)=tag "left_section" undefined
    sub (RightSection _ qop e)=tag "right_section" (sqop qop++" "++sub e)
    sub (RecConstr _ qn fus)=tag "record_construction" undefined
    sub (RecUpdate _ e fus)=tag "record_update" undefined
    sub (EnumFrom _ e)=tag "enum_from" undefined
    sub (EnumFromTo _ e1 e2)=tag "enum_from_to" undefined
    sub (EnumFromThen _ e1 e2)=tag "enum_from_then" undefined
    sub (EnumFromThenTo _ e1 e2 e3)=tag "enum_from_then_to" undefined
    sub (ParArrayFromTo _ e1 e2)=tag "par_array_from_to" undefined
    sub (ParArrayFromThenTo _ e1 e2 e3)=tag "par_array_from_then_to" undefined
    sub (ListComp _ e qss)=tag "list_comp" undefined
    sub (ParComp _ e qss)=tag "parallel_comp" undefined
    sub (ParArrayComp _ e qss)=tag "par_array_comp" undefined
    sub (ExpTypeSig _ e t)=tag "exp_type_sig" $ sub e++"::"++stype t -- e::Type
    sub (VarQuote _ qn)=tag "var_quote" undefined --for Template Haskell
    sub (TypQuote _ qn)=tag "type_quote" undefined
    sub (BracketExp _ b)=tag "bracket_exp" undefined
    sub (SpliceExp _ s)=tag "splice_exp" undefined
    sub (QuasiQuote _ str1 str2)=tag "quasi_quote" undefined
    sub (TypeApp _ t)=tag "type_app" undefined
    sub (XTag _ xn xas e es)=tag "xml_elm" undefined
    sub (XETag _ xn xas e)=tag "xml_elm" undefined
    sub (XPcdata _ str)=tag "PCDATA" undefined
    sub (XExpTag _ e)=tag "escaped_expression" undefined
    sub (XChildTag _ es)=tag "xml_chld_elm" undefined
    sub (CorePragma _ str e)=tag "CORE_pragma" undefined
    sub (SCCPragma _ str e)=tag "SCC_pragma" undefined
    sub (GenPragma _ str (i1,i2) (i3,i4) e)=tag "GENERATED_pragma" undefined
    sub (Proc _ p e)=tag "proc" undefined
    sub (LeftArrApp _ e1 e2)=tag "arrow_app_left" undefined -- exp -< exp
    sub (RightArrApp _ e1 e2)=tag "arrow_app_right" undefined
    sub (LeftArrHighApp _ e1 e2)=tag "higher_order_arrow_app_left" undefined -- exp -<< exp
    sub (RightArrHighApp _ e1 e2)=tag "higher_order_arrow_app_right" undefined
    sub (ArrOp _ e)=tag "arrow_ctrl_op" undefined
    sub (LCase _ as)=tag "case" undefined

stype::Type SrcSpanInfo->String
stype=tag "type" . sub
  where
    sub (TyFun _ t1 t2)=tag "function" $ sub t1++"->"++sub t2
    sub (TyTuple _ b ts)=tag "tuple" $ "("++join "," (map sub ts)++")"
    sub (TyList _ t)=tag "list" $"["++sub t++"]"
    sub (TyApp _ t1 t2)=tag "application" $ sub t1++sub t2
    sub (TyVar _ n)=tag "var" (sname n)
    sub (TyCon _ qn)=tag "constructor" (sqname qn)
    sub (TyParen _ t)="("++sub t++")"
    sub _=undefined

sbinds::Binds SrcSpanInfo->String
sbinds=tag "binds" . sub
  where
    sub (BDecls _ ds)=tag "binding_group" (concatMap sd ds)
    sub (IPBinds _ ipbs)=tag "bg_implicit_params" undefined


smp::ModulePragma SrcSpanInfo->String
smp (LanguagePragma _ names)="<language_pragma>{-# LANGUAGE "++"#-}</language_pragma>\n"
smp _=undefined

sid::ImportDecl SrcSpanInfo->String
sid id=(++"\n") $ tag "import_declaration" $ "import"++tag "import_module" mname
  where
    (ModuleName _ mname) = importModule id

sd::Decl SrcSpanInfo->String
sd (FunBind _ ms)=(++"\n") $ concatMap (tag "fun_bind". sm) ms
  where
    sm (Match _ n ps rhs bind)=(++maybe "" (\b->"where\n"++sbinds b) bind) $ tag "match" $ sname n++join " " (map spat ps)++"="++srhs rhs++"\n"
    sm (InfixMatch _ pl n prs rhs bind)=tag "infix_match" $ spat pl++sname n++join " " (map spat prs)++"="++srhs rhs
sd (PatBind _ p rhs bind)=(++maybe "" (\b->"where\n"++sbinds b) bind) $ tag "pattern_bind" $ spat p++"="++srhs rhs++"\n"
sd (TypeSig _ ns t)=tag "type_signature" $ join "," (map sname ns)++"::"++stype t
sd d=(++"\n") $ tag "unsupported" (show (d::Decl SrcSpanInfo))

ml::ParseResult (Module SrcSpanInfo)->String
ml (ParseOk (Module _ (Just _) mps ids ds))=tag "module" $concatMap smp mps++concatMap sid ids++concatMap sd ds
ml _ = ""

main :: IO ()
main =do
  l<-getArgs
  if null l then return ()
  else putStr . ml =<< parseFile (head l)
