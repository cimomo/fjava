(*
 * Lexer for the simple grammar.
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

{
open Printf
open Symbol
open Location

open Fj_ast
open Fj_ast_parse
open Fj_ast_state
open Fj_ast_exn
open Fj_ast_pos

module Pos = MakePos (struct let name = "Fj_ast_lex" end)
open Pos

(*
 * Current line in the file, and the char position of that line.
 *)
let current_pos = ref (1, 0)

(*
 * Compute the number of '\n' in the string.
 * Also return the char position of the start of the last line.
 * We're given the position of the starting char.
 *)
let line_info s line lchar echar =
   let len = String.length s in
   let rec loop line lchar echar i =
      if i = len then
         line, lchar
      else if s.[i] = '\n' then
         loop (succ line) (succ echar) (succ echar) (succ i)
      else
         loop line lchar (succ echar) (succ i)
   in
      loop line lchar echar 0

(*
 * Get the current lexeme.
 * Set the current file position at the same time.
 *)
let get_lexeme lexbuf =
   (* lexeme and curent file name *)
   let s = Lexing.lexeme lexbuf in
   let file = current_file () in

   (* Get new line position *)
   let line, lchar = !current_pos in
   let line, lchar = line_info s line lchar (Lexing.lexeme_start lexbuf) in

   (* Get start and end char positions *)
   let schar = Lexing.lexeme_start lexbuf - lchar in
   let echar = Lexing.lexeme_end lexbuf - lchar in

   (* Set position, and return lexeme and position *)
   let loc = create_loc (Symbol.add file) line schar line echar in
      current_pos := (line, lchar);
      set_current_location loc;
      s, loc

(*
 * Convert up to 3 octal digits.
 * Return the new position and the octal number.
 *)
let zero_char = Char.code '0'

let char_of_octal s i =
   let len = String.length s in
   let max_index = min (i + 3) len in
   let rec loop code i =
      if i = max_index then
         i, code
      else
         let c = s.[i] in
            match c with
               '0'..'7' -> loop (code * 8 + (Char.code c) - zero_char) (succ i)
             | _ -> i, code
   in
   let i, code = loop 0 i in
      i, Char.chr code

(*
 * Interprete a character escape sequence.
 * Return the new position, and the char.
 *)
let char_of_escape s i =
   match s.[i] with
      '0'..'7' -> char_of_octal s i
    | 'n' -> succ i, '\n'
    | 't' -> succ i, '\t'
    | 'v' -> succ i, '\011'
    | 'b' -> succ i, '\b'
    | 'r' -> succ i, '\r'
    | 'f' -> succ i, '\012'
    | 'a' -> succ i, '\007'
    | c ->   succ i, c

(*
 * Get the character specified by a char constant.
 *)
let lex_char s loc =
   let c = s.[1] in
   let c =
      if c = '\\' then
         snd (char_of_escape s 2)
      else
         c
   in
      TokChar (c, loc)

(*
 * Get the string specified by a string constant.
 *)
let lex_string s loc =
   let max_len = pred (String.length s) in
   let buf = Buffer.create max_len in
   let next_char i =
      let c = s.[i] in
         if c = '\\' then
            char_of_escape s (succ i)
         else
            succ i, c
   in
   let rec loop i =
       if i <> max_len then
          let i, c = next_char i in
             Buffer.add_char buf c;
             loop i
   in
      loop 1;
      TokString (Buffer.contents buf, loc)

(*
 * Keyword table.
 *)
let special =
   ["if",         (fun pos -> TokIf pos);
    "else",       (fun pos -> TokElse pos);
    "for",        (fun pos -> TokFor pos);
    "while",      (fun pos -> TokWhile pos);
    "return",     (fun pos -> TokReturn pos);
    "break",      (fun pos -> TokBreak pos);
    "class",      (fun pos -> TokClass pos);
    "extends",    (fun pos -> TokExtends pos);
    "instanceof", (fun pos -> TokInstanceof pos);
    "try",	  (fun pos -> TokTry pos);
    "catch",      (fun pos -> TokCatch pos);
    "finally",	  (fun pos -> TokFinally pos);
    "public",	  (fun pos -> TokPublic pos);
    "protected",  (fun pos -> TokProtected pos);
    "private",	  (fun pos -> TokPrivate pos);
    "static",	  (fun pos -> TokStatic pos);
    "new",	  (fun pos -> TokNew pos);
    "nil",	  (fun pos -> TokNil pos);
    "true",	  (fun pos -> TokTrue pos);
    "false",	  (fun pos -> TokFalse pos);
    "throw",      (fun pos -> TokThrow pos);
    "(",          (fun pos -> TokLeftParen pos);
    ")",          (fun pos -> TokRightParen pos);
    "[",          (fun pos -> TokLeftBrack pos);
    "]",          (fun pos -> TokRightBrack pos);
    "[]",	  (fun pos -> TokDoubleBrack pos);
    "{",          (fun pos -> TokLeftBrace pos);
    "}",          (fun pos -> TokRightBrace pos);
    ";",          (fun pos -> TokSemi pos);
    ",",          (fun pos -> TokComma pos);
    ".",          (fun pos -> TokDot pos);
    "?",          (fun pos -> TokQuest pos);
    ":",          (fun pos -> TokColon pos);
    "*",          (fun pos -> TokStar pos);
    "%",          (fun pos -> TokPercent pos);
    "<=",         (fun pos -> TokLe pos);
    ">=",         (fun pos -> TokGe pos);
    "!=",         (fun pos -> TokNotEq pos);
    "==",         (fun pos -> TokEqEq pos);
    "=",          (fun pos -> TokEq pos);
    "!",          (fun pos -> TokBang pos);
    "+",          (fun pos -> TokPlus pos);
    "-",          (fun pos -> TokMinus pos);
    "*",          (fun pos -> TokStar pos);
    "/",          (fun pos -> TokSlash pos);
    "<",          (fun pos -> TokLt pos);
    ">",          (fun pos -> TokGt pos);
    "->",         (fun pos -> TokRightArrow pos);
    "&",          (fun pos -> TokAmp pos);
    "|",          (fun pos -> TokPipe pos);
    "^",          (fun pos -> TokHat pos);
    "&&",         (fun pos -> TokLAnd pos);
    "||",         (fun pos -> TokLOr pos);
    "<<",         (fun pos -> TokLsl pos);
    ">>",         (fun pos -> TokAsr pos);
    ">>>",        (fun pos -> TokLsr pos);
    "++",         (fun pos -> TokPlusPlus pos);
    "--",         (fun pos -> TokMinusMinus pos)]

let symtab =
   List.fold_left (fun table (name, f) ->
      SymbolTable.add table (Symbol.add name) f) SymbolTable.empty special

(*
 * Look up the string, and return a symbol if
 * the lookup fails.
 *)
let lex_symbol s loc =
   let sym = Symbol.add s in
      try (SymbolTable.find symtab sym) loc with
         Not_found ->
            TokId (sym, loc)

(*
 * Look up the operator.
 * Syntax error if the lookup fails.
 *)
let lex_operator s loc =
   let sym = Symbol.add s in
      try (SymbolTable.find symtab sym) loc with
         Not_found ->
            let pos = string_pos "lex_operator" (loc_pos loc) in
               raise (AstException (pos, StringError ("illegal operator: " ^ s)))
}

(*
 * Comments and white space.
 *)
let white1 = [' ' '\t' '\n']
let white2 = "//" [^ '\n']* '\n'
let white3 = "/*" (('*' [^'/'] | [^'*'] '/' | [^'*' '/'])* | ['*' '/']) "*/"
let white = white1 | white2 | white3

(*
 * Names.
 *)
let name_prefix = ['_' 'A'-'Z' 'a'-'z']
let name_suffix = ['_' 'A'-'Z' 'a'-'z' '0'-'9']
let name = name_prefix name_suffix*

(*
 * Operators and special characters.
 * XXX: '^' added by turtles. -kchen
 *)
let special1 = ['(' ')' '[' ']' '{' '}' ';' ',' '.' '?' ':' '*' '%' '^']
let special2 = ['<' '>' '!' '='] '='
let special3 = ['=' '!']
let special4 = ['+' '-' '/' '<' '>' '&' '|']+
let operator = special1 | special2 | special3 | special4

(*
 * Treat the empty brackets as a special operator.
 *)
let brackets = '[' white* ']'

(*
 * Integers.
 *)
let odigit = ['0'-'7']
let octal = '0' odigit*
let decimal = ['0'-'9']+
let hex = "0x" ['0'-'9' 'a'-'f' 'A'-'F']+

(*
 * Floating point numbers.
 *)
let float0 = ['0'-'9']+ '.' ['0'-'9']* (('e' | 'E') ('+' | '-')? decimal)?
let float1 = ['0'-'9']* '.' ['0'-'9']+ (('e' | 'E') ('+' | '-')? decimal)?
let float2 = ['0'-'9']+ (('e' | 'E') ('+' | '-')? decimal)
let float = float0 | float1 | float2

(*
 * Strings and chars.
 *)
let charcode = '\\' odigit odigit? odigit?
let charesc = '\\' ['n' 't' 'v' 'b' 'o' 'r' 'a' '\\' '\'']
let charconst = '\'' (charesc | charcode | [^'\\']) '\''
let stringconst = '"' ([^'"'] | "\\\"")* '"'

(*
 * Main lexer.
 *)
rule main = parse
   white+
      { ignore (get_lexeme lexbuf); main lexbuf }

   (* Numbers *)
 | octal
      { let s, pos = get_lexeme lexbuf in
           TokInt (int_of_string ("0o" ^ s), pos)
      }
 | hex
      { let s, pos = get_lexeme lexbuf in
           TokInt (int_of_string s, pos)
      }
 | decimal
      { let s, pos = get_lexeme lexbuf in
           TokInt (int_of_string s, pos)
      }
 | float
      { let s, pos = get_lexeme lexbuf in
           TokFloat (float_of_string s, pos)
      }

   (* Names *)
 | name
      { let id, pos = get_lexeme lexbuf in
           lex_symbol id pos
      }

   (* Operators *)
 | brackets
      { let _, pos = get_lexeme lexbuf in
           lex_operator "[]" pos
      }
 | operator
      { let id, pos = get_lexeme lexbuf in
           lex_operator id pos
      }

   (* Strings and chars *)
 | stringconst
      { let s, pos = get_lexeme lexbuf in
           lex_string s pos
      }
 | charconst
      { let s, pos = get_lexeme lexbuf in
           lex_char s pos
      }

   (* All else is a syntax error *)
 | _
      { let s, loc = get_lexeme lexbuf in
        let pos = string_pos "lexer" (loc_pos loc) in
           raise (AstException (pos, StringError (Printf.sprintf "illegal char: '%s'" (String.escaped s))))
      }

 | eof
      { TokEof }


(*
 * -*-
 * Local Variables:
 * Caml-master: "set"
 * End:
 * -*-
 *)
