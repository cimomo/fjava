(*
 * FJava intermediate representation.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) Kai Chen
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *)
open Symbol
open Field_table

(*
 * A variable is just a symbol.
 *)
type var = symbol
type ty_var = symbol

(*
 * Types.
 *)
type ty =
   (* Basic types *)
 | TyUnit
 | TyNil
 | TyBool
 | TyChar
 | TyString
 | TyInt
 | TyFloat

   (* Arrays of values, all elements have the same type *)
 | TyArray of ty

   (* Functions have a list of types for the arguments, and a result type *)
 | TyFun of ty list * ty

   (* A method has a "this" type, some args, and a result *)
 | TyMethod of (ty * ty list * ty)

   (* TyObject is an instance of a class *)
 | TyObject of ty_var

(*
 * A class has:
 *
 * A parent class.  Only the built-in FjObject class
 * does not have a parent.
 *
 * A list of interfaces.  This includes all the
 * interfaces, including those defined by the parent.
 *
 *
 * An initialization function.  This is a function
 * that sets the initial values for the fields.
 *
 * A set of constructors.
 *
 * The method and field lists hold _all_ of the methods
 * and fields, including those defined by the parent.
 *)
type class_info =
   { class_parents    : var list;
     class_consts     : (var * ty) list;
     class_methods    : ty FieldMTable.t;
     class_fields     : ty FieldMTable.t;
   }

(*
 * Typedefs are used to define classes and types
 * at the program level.
 *)
type tydef =
   TyDefClass of class_info option

(*
 * Unary operators.
 *)
type unop =
   (* Arithmetic *)
   UMinusIntOp
 | UMinusFloatOp
 | UNotIntOp
 | UNotBoolOp

   (* Numeric coercions *)
 | UIntOfFloat
 | UFloatOfInt
 | UCharOfInt
 | UIntOfChar

(*
 * Binary operators.
 *)
type binop =
   (* Integers *)
   AddIntOp
 | SubIntOp
 | MulIntOp
 | DivIntOp
 | RemIntOp
 | AndIntOp
 | OrIntOp
 | LslIntOp    (* Logical shift left << *)
 | LsrIntOp    (* Logical shift right >> *)
 | AsrIntOp    (* Arithmetic shift right >>> *)
 | XorIntOp
 | EqIntOp
 | NeqIntOp
 | LeIntOp
 | LtIntOp
 | GtIntOp
 | GeIntOp

   (* Floats *)
 | AddFloatOp
 | SubFloatOp
 | MulFloatOp
 | DivFloatOp
 | RemFloatOp
 | EqFloatOp
 | NeqFloatOp
 | LeFloatOp
 | LtFloatOp
 | GtFloatOp
 | GeFloatOp
 
   (* Bools *)
 | EqBoolOp
 | NeqBoolOp

(*
 * Values.
 *)
type atom =
   AtomUnit
 | AtomNil
 | AtomBool  of bool
 | AtomChar  of char
 | AtomInt   of int
 | AtomFloat of float
 | AtomVar   of var

(*
 * Function classes:
 *    FunGlobalClass: any user-defined function that is not a method  (not 
 *          necessarily with global scope)
 *    FunLocalClass: local (compiler-defined) function
 *
 * Note that return statements are associated with global/method functions,
 * never with local functions.
 *)
type fun_class =
   FunGlobalClass
 | FunMethodClass
 | FunLocalClass

(*
 * Intermediate code expressions.
 *)
type exp =
   (* Mutually recursive function definitions.  They are in scope for each
      function's body and the duration of exp. *)
   LetFuns of fundef list * exp

   (* Arithmetic *)
   (* Functional Variable/atom creation (non-destructive) *)
 | LetVar of var * ty * atom * exp
 | LetAtom of var * ty * atom * exp
 | LetUnop of var * ty * unop * atom * exp
 | LetBinop of var * ty * binop * atom * atom * exp

   (* Functions *)
   (* let v1 : ty = v2(a1, ..., aN) in exp *)
 | LetApply of var * ty * var * atom list * exp
   (* You pass the pointer to self explicitly when calling a method:
      let v1 : ty = v2[a_this](a1, ..., aN) in exp *)
 | LetApplyMethod of var * ty * var * atom * atom list * exp
   (* External functions need to have their types explicitly annotated.
      let v1 : ty1 = External_fun["f_name", ext_fun_ty](a1, ... , aN) in exp *)
 | LetExt of var * ty * string * ty * atom list * exp
 | TailCall of var * atom list
   (* var = the function being returned from.  atom = value returned *)
 | Return of var * atom

   (* Conditional *)
 | IfThenElse of atom * exp * exp

   (* Exception handling *)
   (* if e1 throws an exception, bind it to var and execute e2 *)
 | Try of exp * var * exp
 | Raise of atom
   (* TypeCase is like a bunch of if/then/else statements for switching based on
      the run-time type of an object:
      if a instanceof ty_var_1 then
         let var_1 : ty_var_1 = a in exp_1
      else if a instanceof ty_var_2 then
         ...
      else
         exp (* The last exp, outside of the list *) 
    *)
 | TypeCase of atom * (ty_var * var * exp) list * exp

   (* Destructive (non-functional) variable assignment 
      v : ty <- a; exp *)
 | SetVar of var * ty * atom * exp

   (* Array/pointer operations *)
   (* a1[a2] : ty <- a3; exp *)
 | SetSubscript of atom * atom * ty * atom * exp
   (* let v : ty = a1[a2] in exp *)
 | LetSubscript of var * ty * atom * atom * exp

   (* Record projection *)
   (* a1.var : ty <- a2; exp *)
 | SetProject of atom * var * ty * atom * exp
   (* let v1 : ty = a.v2 in exp *)
 | LetProject of var * ty * atom * var * exp

   (* Allocation *)
 | LetString of var * string * exp
   (* atom list is list of array dimensions. Other atom is the value to
      initialize the array elements with *)
 | LetArray of var * ty * atom list * atom * exp
   (* let v1 : ty = new v2 in exp *)
 | LetObject of var * ty * ty_var * exp

(*
 * A function definition has:
 *    1. a function classification  (global, method, or local)
 *    2. the function type
 *    3. then formal parameters
 *    4. the body
 *)
and fundef = var * fun_info

and fun_info = fun_class * ty * var list * exp

(*
 * A program has the following parts:
 *   prog_types: type definitions
 *   prog_abstypes: abstract type identifiers
 *   prog_funs: a set of function definitions
 *   prog_main: the name of the main function
 *   prog_object: the name of the FjObject type
 *)
type prog =
   { prog_types    : tydef SymbolTable.t;
     prog_abstypes : SymbolSet.t;
     prog_funs     : fun_info SymbolTable.t;
     prog_main     : var;
     prog_object   : var
   }

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
