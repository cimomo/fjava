%{
open Symbol
open Location

open Fj_ast
open Fj_ast_util
open Fj_ast_state
open Fj_ast_exn
open Fj_ast_pos

module Pos = MakePos (struct let name = "Fj_ast_parse" end)
open Pos

(*
 * The name of the root class.
 *)
let object_sym = Symbol.add "FjObject"

(*
 * Var name in a declaration.
 *)
type var_name =
   VarNameId of symbol * loc
 | VarNameArray of var_name * loc
 | VarNameFun of var_name * (symbol * ty) list * loc

let loc_of_var_name = function
   VarNameId (_, loc) -> loc
 | VarNameArray (_, loc) -> loc
 | VarNameFun (_, _, loc) -> loc

(*
 * This is a temporary hack because new arrays do
 * not allow arbitrary types.
 *)
let var_of_var_name = function
   VarNameId (id, loc) ->
      id, loc
 | VarNameArray (_, loc)
 | VarNameFun (_, _, loc) ->
      let pos = string_pos "var_of_var_name" (loc_pos loc) in
         raise (AstException (pos, StringError "complex array allocations not implemented"))

(*
 * Build a variable name from an id and a nest tree.
 *)
let make_var_name id nest loc =
   let rec collect name = function
      0 -> name
    | i -> collect (VarNameArray (name, loc)) (pred i)
   in
      collect (VarNameId (id, loc)) nest

(*
 * Do the same for types.
 *)
let make_type id nest loc =
   let rec collect ty = function
      0 -> ty
    | i -> collect (TypeArray (ty, loc)) (pred i)
   in
      collect (TypeId (id, loc)) nest

(*
 * Build a variable declaration from the syntax.
 *)
let rec make_var_decl ty = function
   VarNameId (n, loc) ->
      n, ty, loc
 | VarNameArray (v, loc) ->
      make_var_decl (TypeArray (ty, loc)) v
 | VarNameFun (v, args, loc) ->
      make_var_decl (TypeFun (List.map snd args, ty, loc)) v

and make_var_decls ty decls =
   List.map (make_var_decl ty) decls

(*
 * Make a param decl from a var decl.
 *)
let make_param_decls args =
   List.map (fun (v, ty, _) -> v, ty) args

(*
 * Build a variable declaration from the syntax.
 *)
let make_var_init_decls ty defs =
   (* Build the declaration with an initializer *)
   let rec make_def ty e = function
      VarNameId (n, loc) ->
         n, ty, e, loc
    | VarNameArray (v, loc) ->
         make_def (TypeArray (ty, loc)) e v
    | VarNameFun (v, args, loc) ->
         make_def (TypeFun (List.map snd args, ty, loc)) e v
   in

   (* Initial type *)
   let make_init_def (v, e) =
      make_def ty e v
   in
      List.map make_init_def defs

(*
 * A function definition.
 *)
let get_fun_var (v, ty) =
   v, ty, loc_of_type ty

let make_fun_def ty decl body loc =
   let loc = union_loc (loc_of_type ty) loc in
      match decl with
         VarNameFun (res, vars, _) ->
            let vars = List.map get_fun_var vars in
            let f, ty, _ = make_var_decl ty res in
               FunDefs ([f, vars, ty, SeqExp (body, loc), loc], loc)
       | VarNameId _
       | VarNameArray _ ->
	    let pos = string_pos "make_fun_def" (loc_pos loc) in
               raise (AstException (pos, StringError "not a function"))

(*
 * Constructor definition.
 *)
let make_const_def decl body loc =
   match decl with
      VarNameFun (res, vars, _) ->
         let vars = List.map get_fun_var vars in
         let f, _, _ = make_var_decl (TypeVoid loc) res in
            ConstDef (f, vars, SeqExp (body, loc), loc)
    | VarNameId _
    | VarNameArray _ ->
	 let pos = string_pos "make_fun_def" (loc_pos loc) in
            raise (AstException (pos, StringError "not a function"))

(*
 * Unary expression.
 *)
let make_unop op expr =
   UnOpExp (op, expr, loc_of_exp expr)

(*
 * Binary expressions.
 *)
let make_binop op expr1 expr2 =
   let loc = union_loc (loc_of_exp expr1) (loc_of_exp expr2) in
      BinOpExp (op, expr1, expr2, loc)

let make_boolop op expr1 expr2 =
   let loc = union_loc (loc_of_exp expr1) (loc_of_exp expr2) in
      BoolOpExp (op, expr1, expr2, loc)

(*
 * Pre and loc increment.
 *)
let make_uarith_op loc op expr =
   let loc = union_loc loc (loc_of_exp expr) in
      UArithExp (op, expr, loc)

(*
 * Optional expression.
 *)
let make_opt_expr opt_expr def_expr =
   match opt_expr with
      Some expr -> expr
    | None -> def_expr
%}

/*
 * End-of-file is a token.
 */
%token TokEof

/*
 * Binary operators return locition.
 */
%token <Location.loc> TokPlus
%token <Location.loc> TokMinus
%token <Location.loc> TokPlusPlus
%token <Location.loc> TokMinusMinus
%token <Location.loc> TokStar
%token <Location.loc> TokSlash
%token <Location.loc> TokPercent
%token <Location.loc> TokLAnd
%token <Location.loc> TokLOr
%token <Location.loc> TokLsl
%token <Location.loc> TokLsr
%token <Location.loc> TokAsr
%token <Location.loc> TokEq
%token <Location.loc> TokEqEq
%token <Location.loc> TokNotEq
%token <Location.loc> TokLt
%token <Location.loc> TokLe
%token <Location.loc> TokGe
%token <Location.loc> TokGt
%token <Location.loc> TokAmp
%token <Location.loc> TokPipe
%token <Location.loc> TokHat
%token <Fj_ast.binop * Location.loc> TokAssign

/*
 * Keywords.
 */
%token <Location.loc> TokTypedef
%token <Location.loc> TokIf
%token <Location.loc> TokElse
%token <Location.loc> TokFor
%token <Location.loc> TokWhile
%token <Location.loc> TokReturn
%token <Location.loc> TokBreak
%token <Location.loc> TokClass
%token <Location.loc> TokExtends
%token <Location.loc> TokInstanceof
%token <Location.loc> TokTry
%token <Location.loc> TokCatch
%token <Location.loc> TokFinally
%token <Location.loc> TokPublic
%token <Location.loc> TokProtected
%token <Location.loc> TokPrivate
%token <Location.loc> TokStatic
%token <Location.loc> TokNew
%token <Location.loc> TokNil
%token <Location.loc> TokTrue
%token <Location.loc> TokFalse
%token <Location.loc> TokThrow

%token <Location.loc> TokBang
%token <Location.loc> TokQuest
%token <Location.loc> TokColon
%token <Location.loc> TokSemi
%token <Location.loc> TokComma
%token <Location.loc> TokDot
%token <Location.loc> TokRightArrow
%token <Location.loc> TokLeftParen
%token <Location.loc> TokRightParen
%token <Location.loc> TokLeftBrack
%token <Location.loc> TokRightBrack
%token <Location.loc> TokDoubleBrack
%token <Location.loc> TokLeftBrace
%token <Location.loc> TokRightBrace

/*
 * Terminal tokens.
 */
%token <Symbol.symbol * Location.loc> TokId
%token <string * Location.loc> TokString
%token <char * Location.loc> TokChar
%token <int * Location.loc> TokInt
%token <float * Location.loc> TokFloat

/*
 * Precedences.
 */
%nonassoc TokInstanceof
%left TokComma
%right TokEq
%left TokLOr
%left TokLAnd
%left TokPipe
%left TokHat
%left TokAmp
%left TokEqEq TokNotEq
%left TokLe TokLt TokGe TokGt
%left TokLsl TokLsr TokAsr
%left TokPlus TokMinus
%left TokStar TokSlash TokPercent
%nonassoc TokThrow
%right prec_unary prec_cast TokPlusPlus TokMinusMinus
%left prec_apply prec_subscript TokDot TokLeftParen TokLeftBrack

/* Eliminate the if/then shift/reduce conflict */
%right      TokWhile TokFor
%nonassoc   prec_ifthen
%nonassoc   TokElse prec_ifthenelse
%nonassoc   TokTry
%left       TokCatch TokFinally

/*
 * A complete program.
 */
%start prog
%type <Fj_ast.prog> prog

%%

/************************************************************************
 * TOPLEVEL PRODUCTIONS
 ************************************************************************/

/*
 * A program is a sequence of
 * definitions.
 */
prog:     class_defs TokEof
          { $1 }
        ;

/************************************************************************
 * CLASS DEFINITIONS
 ************************************************************************/

/*
 * Definitions.
 */
class_defs:
          rev_class_defs
          { List.rev $1 }
        ;

rev_class_defs:
          /* empty */
          { [] }
        | rev_class_defs mod_class_def
          { $2 :: $1 }
        ;

mod_class_def:
          type_mods class_def
          { $2 }
        ;

/*
 * A class def has the class keyword,
 * then an optional extends modifier,
 * then an optional implements modifier,
 * the the class body.
 */
class_def:
          TokClass TokId opt_extends TokLeftBrace class_body TokRightBrace
          { let loc = union_loc $1 $6 in
            let v, _ = $2 in
               ClassDef (v, $3, $5, loc)
          }
        ;

opt_extends:
          /* empty */
          { object_sym }
        | TokExtends TokId
          { fst $2 }
        ;

/*
 * A class body has a set of fields.
 */
class_body:
          rev_member_defs
          { List.rev $1 }
        ;

rev_member_defs:
          /* empty */
          { [] }
        | rev_member_defs type_mods member_def
          { $3 :: $1 }
        ;

member_def:
          var_defs
          { $1 }
        | fun_def
          { $1 }
        | const_def
          { $1 }
        ;

/************************************************************************
 * DECLARATIONS
 ************************************************************************/

/*
 * Standard type modifiers.
 * We just ignore all of these keywords.
 */
type_mods:
          /* empty */
          { () }
        | type_mods type_mod
          { () }
        ;

type_mod: TokPublic
          { () }
        | TokProtected
          { () }
        | TokPrivate
          { () }
        | TokStatic
          { () }
        ;

/*
 * A type specifier is just an id followed by some optional brackets.
 */
type_spec:
          id_brackets
          { let id, nest, loc = $1 in
               make_type id nest loc
          }
         ;

/*
 * An identifier followed by some brackets.
 * We have this strange form to prevent shift/reduce
 * conflicts with array subscripting, which requires
 * an expression between the brackets.
 */
id_brackets:
          TokId
          { let id, loc = $1 in
               id, 0, loc
          }
        | TokId brackets
          { let id, loc1 = $1 in
            let nest, loc2 = $2 in
            let loc = union_loc loc1 loc2 in
               id, nest, loc
          }
        ;

brackets:
	  TokDoubleBrack
          { 1, $1 }
        | brackets TokDoubleBrack
          { let count, loc1 = $1 in
              succ count, union_loc loc1 $2
          }
        ;

/*
 * Variable declarations.
 * No initializer is allowed.
 */
opt_var_decl_list:
          /* empty */
          { [] }
        | var_decl_list
          { $1 }
        ;

var_decl_list:
          rev_var_decl_list
          { List.rev $1 }
        ;

rev_var_decl_list:
          var_decl
          { [$1] }
        | rev_var_decl_list TokComma var_decl
          { $3 :: $1 }
        ;

var_decl:
          type_spec direct_decl
          { make_var_decl $1 $2 }
        ;

/*
 * Variable definitions.
 */
var_defs:
          type_spec init_def_list TokSemi
          { let loc = union_loc (loc_of_type $1) $3 in
               VarDefs (make_var_init_decls $1 $2, loc)
          }
        ;

/*
 * Declarator with an optional initializer.
 */
init_def_list:
          rev_init_def_list
          { List.rev $1 }
        ;

rev_init_def_list:
          init_def
          { [$1] }
        | rev_init_def_list TokComma init_def
          { $3 :: $1 }
        ;

init_def:
          direct_decl
          { $1, None }
        | direct_decl TokEq expr
          { $1, Some $3 }
        ;

/*
 * Declarators without initializers.
 * Again, we have this strange form to prevent conflicts
 * with array subscripting.
 */
direct_decl:
          id_brackets
          { let id, nest, loc = $1 in
               make_var_name id nest loc
          }
        | fun_decl
          { $1 }
        ;

fun_decl:
          id_brackets TokLeftParen opt_var_decl_list TokRightParen
          { let id, nest, loc1 = $1 in
            let name = make_var_name id nest loc1 in
            let loc = union_loc loc1 $4 in
               VarNameFun (name, make_param_decls $3, loc)
          }
        | fun_decl TokDoubleBrack %prec prec_subscript
          { let loc = union_loc (loc_of_var_name $1) $2 in
               VarNameArray ($1, loc)
          }
        | fun_decl TokLeftParen opt_var_decl_list TokRightParen %prec prec_apply
          { let loc = union_loc (loc_of_var_name $1) $4 in
               VarNameFun ($1, make_param_decls $3, loc)
          }
        ;

/*
 * Declare a function.
 */
fun_def:  type_spec direct_decl TokLeftBrace stmt_list TokRightBrace
          { make_fun_def $1 $2 $4 $5 }
        ;

/*
 * A constructor is like a function,
 * but it has no type.
 */
const_def:
          direct_decl TokLeftBrace stmt_list TokRightBrace
          { make_const_def $1 $3 $4 }
        ;

/************************************************************************
 * EXPRESSIONS
 ************************************************************************/

/*
 * Expessions.
 */
expr:     ncast_expr
          { $1 }
        | cast_expr
          { $1 }
        ;

ncast_expr:
          TokInt
          { let i, loc = $1 in
               IntExp (i, loc)
          }
        | TokFloat
          { let x, loc = $1 in
               FloatExp (x, loc)
          }
        | TokChar
          { let c, loc = $1 in
               CharExp (c, loc)
          }
        | TokString
          { let s, loc = $1 in
               StringExp (s, loc)
          }
        | TokNil
          { NilExp $1 }
        | TokTrue
          { BoolExp (true, $1) }
        | TokFalse
          { BoolExp (false, $1) }
        | TokMinus expr %prec prec_unary
          { make_unop UMinusOp $2 }
        | TokBang expr %prec prec_unary
          { make_unop UNotOp $2 }
        | TokPlusPlus expr %prec prec_unary
          { make_uarith_op $1 PreIncrOp $2 }
        | TokMinusMinus expr %prec prec_unary
          { make_uarith_op $1 PreDecrOp $2 }
        | expr TokPlusPlus %prec prec_unary
          { make_uarith_op $2 PostIncrOp $1 }
        | expr TokMinusMinus %prec prec_unary
          { make_uarith_op $2 PostDecrOp $1 }
        | expr TokPlus expr
          { make_binop PlusOp $1 $3 }
        | expr TokMinus expr
          { make_binop MinusOp $1 $3 }
        | expr TokStar expr
          { make_binop TimesOp $1 $3 }
        | expr TokSlash expr
          { make_binop DivideOp $1 $3 }
        | expr TokPercent expr
          { make_binop ModOp $1 $3 }
        | expr TokLsl expr
          { make_binop LslOp $1 $3 }
        | expr TokLsr expr
          { make_binop LsrOp $1 $3 }
        | expr TokAsr expr
          { make_binop AsrOp $1 $3 }
        | expr TokLAnd expr
          { make_boolop LAndOp $1 $3 }
        | expr TokLOr expr
          { make_boolop LOrOp $1 $3 }
        | expr TokEqEq expr
          { make_binop EqOp $1 $3 }
        | expr TokNotEq expr
          { make_binop NeqOp $1 $3 }
        | expr TokLe expr
          { make_binop LeOp $1 $3 }
        | expr TokLt expr
          { make_binop LtOp $1 $3 }
        | expr TokGe expr
          { make_binop GeOp $1 $3 }
        | expr TokGt expr
          { make_binop GtOp $1 $3 }
          /* XXX: added bit-wise AND, OR, XOR  -kchen */
        | expr TokAmp expr
          { make_binop BAndOp $1 $3 }
        | expr TokPipe expr
          { make_binop BOrOp $1 $3 }
        | expr TokHat expr
          { make_binop BXorOp $1 $3 }
        | expr TokEq expr
          { let loc = union_loc (loc_of_exp $1) (loc_of_exp $3) in
               AssignExp (None, $1, $3, loc)
          }
        | expr binop TokEq expr %prec TokEq
          { let loc = union_loc (loc_of_exp $1) (loc_of_exp $4) in
               AssignExp (Some $2, $1, $4, loc)
          }
        | expr TokInstanceof TokId
          { let id, loc = $3 in
            let loc = union_loc (loc_of_exp $1) loc in
               InstanceofExp ($1, id, loc)
          }
        | TokNew new_expr
          { $2 }
	| TokThrow expr
	  { let loc = union_loc $1 (loc_of_exp $2) in
               ThrowExp ($2, loc)
          }
        ;

cast_expr:
	  TokId
          { let id, loc = $1 in
               VarExp (id, loc)
          }
        | cast_expr TokDot TokId
          { let loc = union_loc (loc_of_exp $1) (snd $3) in
               ProjectExp ($1, fst $3, loc)
          }
        | cast_expr TokLeftBrack expr TokRightBrack %prec prec_subscript
          { let loc = union_loc (loc_of_exp $1) $4 in
               SubscriptExp ($1, $3, loc)
          }
        | cast_expr TokLeftParen arg_list TokRightParen %prec prec_apply
          { let loc = union_loc (loc_of_exp $1) $4 in
               ApplyExp ($1, $3, loc)
          }
	| TokLeftParen TokId TokRightParen cast_expr %prec prec_cast
          { let id, loc2 = $2 in
            let ty = make_type id 0 loc2 in
            let loc = union_loc $1 (loc_of_exp $4) in
               CastExp (ty, $4, loc)
          }
	| TokLeftParen TokId brackets TokRightParen cast_expr %prec prec_cast
          { let id, loc2 = $2 in
            let nest, loc3 = $3 in
            let ty = make_type id nest (union_loc loc2 loc3) in
            let loc = union_loc $1 (loc_of_exp $5) in
               CastExp (ty, $5, loc)
          }
	| TokLeftParen ncast_expr TokRightParen %prec prec_apply
          { $2 }
	| TokLeftParen TokId TokRightParen %prec prec_apply
          { let id, loc = $2 in
               VarExp (id, loc)
          }
        ;

/*
 * New is followed by either an array
 * or constructor declaration.  Note that
 * this is more general than what was given
 * in class.
 */
new_expr:
	  new_array rev_brackets
          { let id, loc1 = var_of_var_name $1 in
            let dimens, loc2 = $2 in
            let loc = union_loc loc1 loc2 in
               NewArrayExp (id, List.rev dimens, loc)
          }
        | TokId TokLeftParen rev_arg_list TokRightParen
          { let id, loc1 = $1 in
            let loc = union_loc loc1 $4 in
               NewConstExp (id, List.rev $3, loc)
          }
        | new_const
          { let id, loc = $1 in
               NewConstExp (id, [], loc)
          }
        ;

new_array: new_type
           { $1 }
         | TokId
           { let id, loc = $1 in
                VarNameId (id, loc)
           }
         ;

new_type:  new_type TokDoubleBrack
           { let loc = union_loc (loc_of_var_name $1) $2 in
               VarNameArray ($1, loc)
           }
         | new_type TokLeftParen opt_var_decl_list TokRightParen
           { let loc = union_loc (loc_of_var_name $1) $4 in
               VarNameFun ($1, make_param_decls $3, loc)
           }
         | TokId TokDoubleBrack
           { let id, loc1 = $1 in
             let loc = union_loc loc1 $2 in
                VarNameArray (VarNameId (id, loc1), loc)
           }
         | TokId TokLeftParen var_decl_list TokRightParen
           { let id, loc1 = $1 in
             let loc = union_loc loc1 $4 in
                VarNameFun (VarNameId (id, loc1), make_param_decls $3, loc)
           }
	 | new_const
	   { let id, loc = $1 in
               VarNameFun (VarNameId (id, loc), [], loc)
           }
         ;

new_const: TokId TokLeftParen TokRightParen
           { $1 }
         ;

rev_brackets:
          TokLeftBrack expr TokRightBrack %prec prec_subscript
          { let loc = union_loc $1 $3 in
               [$2], loc
          }
        | rev_brackets TokLeftBrack expr TokRightBrack %prec prec_subscript
          { let args, loc1 = $1 in
            let loc = union_loc loc1 $4 in
               $3 :: args, loc
          }
        ;

/*
 * An optional expression.
 */
opt_expr: /* empty */
          { None }
        | expr
          { Some $1 }
        ;

/*
 * A statement is a terminated expression.
 */
stmt:     TokSemi
          { SeqExp ([], $1) }
        | expr TokSemi
          { $1 }
        | TokIf TokLeftParen expr TokRightParen stmt %prec prec_ifthen
          { let loc = union_loc $1 (loc_of_exp $5) in
               IfExp ($3, $5, None, loc)
          }
        | TokIf TokLeftParen expr TokRightParen stmt TokElse stmt %prec prec_ifthenelse
          { let loc = union_loc $1 (loc_of_exp $7) in
               IfExp ($3, $5, Some $7, loc)
          }
        | TokFor TokLeftParen opt_expr TokSemi opt_expr TokSemi opt_expr TokRightParen stmt %prec TokFor
          { let loc = union_loc $1 (loc_of_exp $9) in
            let def_expr = IntExp (1, loc) in
            let init = make_opt_expr $3 def_expr in
            let test = make_opt_expr $5 def_expr in
            let step = make_opt_expr $7 def_expr in
               ForExp (init, test, step, $9, loc)
          }
        | TokWhile TokLeftParen expr TokRightParen stmt %prec TokWhile
          { let loc = union_loc $1 (loc_of_exp $5) in
               WhileExp ($3, $5, loc)
          }
        | TokReturn expr TokSemi
          { let loc = union_loc $1 (loc_of_exp $2) in
               ReturnExp ($2, loc)
          }
        | TokBreak TokSemi
          { let loc = union_loc $1 $2 in
               BreakExp loc
          }
        | TokLeftBrace stmt_list TokRightBrace
          { let loc = union_loc $1 $3 in
               SeqExp ($2, loc)
          }
        | TokTry TokLeftBrace stmt_list TokRightBrace rev_catches opt_finally %prec TokTry
          { let loc = union_loc $1 $4 in
               TryExp (SeqExp ($3, loc), List.rev $5, $6, loc)
          }
        | var_defs
          { let loc = loc_of_def $1 in
               DefExp ($1, loc)
          }
        | fun_def
          { let loc = loc_of_def $1 in
               DefExp ($1, loc)
          }
        ;

rev_catches:
          /* empty */
          { [] }
        | rev_catches TokCatch TokLeftParen TokId TokId TokRightParen TokLeftBrace stmt_list TokRightBrace %prec TokCatch
          { let loc = union_loc $7 $9 in
               (fst $4, fst $5, SeqExp ($8, loc)) :: $1
          }
        ;

opt_finally:
          /* empty */
          { None }
        | TokFinally TokLeftBrace stmt_list TokRightBrace %prec TokFinally
          { let loc = union_loc $1 $4 in
               Some (SeqExp ($3, loc))
          }
        ;

stmt_list:
          rev_stmt_list
          { List.rev $1 }
        ;

rev_stmt_list:
          /* empty */
          { [] }
        | rev_stmt_list stmt
          { $2 :: $1 }
        ;

/*
 * Arguments are comma-separated list of expressions.
 */
arg_list: /* empty */
          { [] }
        | rev_arg_list
          { List.rev $1 }
        ;

rev_arg_list:
          expr
          { [$1] }
        | rev_arg_list TokComma expr
          { $3 :: $1 }
        ;

/*
 * binary operator.
 */
binop:    TokPlus
          { PlusOp }
        | TokMinus
          { MinusOp }
        | TokStar
          { TimesOp }
        | TokSlash
          { DivideOp }
        | TokPercent
          { ModOp }
        | TokAmp
          { BAndOp }
        | TokPipe
          { BOrOp }
        | TokHat
          { BXorOp }
        | TokLsl
          { LslOp }
        | TokLsr
          { LsrOp }
        | TokAsr
          { AsrOp }
        ;
