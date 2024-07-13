(** This modules defines a set of regular expressions for ocammlex. *)

{
    open Lexing
    open Parser

    exception SyntaxError of string

    let next_line lexbuf =
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- { 
            pos with pos_bol = lexbuf.lex_curr_pos;
            pos_lnum = pos.pos_lnum + 1
        }
}

(* Helper regexes *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let character = '\'' alpha '\''
let greek = "a" | "α" | "β" | "γ" | "δ" | "ε" | "ζ" | "η" | "θ" | "ι" | "κ" | "λ" | "μ" | "ν" | "ξ" | "ο" | "π" | "ρ" | "σ" 
    | "τ" | "υ" | "φ" | "χ" | "ψ" | "ω" | "Ά" | "Έ" | "Ή" | "Ί" | "Ό" | "Ύ" | "Ώ" | "ά" | "έ" | "ή" | "ί" | "ϊ" | "ϋ" | "ό" 
    | "ύ" | "ώ" | "ϐ" | "ϑ" | "ϒ" | "ϓ" | "ϔ" | "ϕ" | "ϖ" | "ϗ" | "Ϙ" | "ϙ" | "Ϛ" | "ϛ" | "Ϝ" | "ϝ" | "Ϟ" | "ϟ" | "Ϡ" | "ϡ" 
    | "Ϣ" | "ϣ" | "Ϥ" | "ϥ" | "Ϧ" | "ϧ" | "Ϩ" | "ϩ" | "Ϫ" | "ϫ" | "Ϭ" | "ϭ" | "Ϯ" | "ϯ" | "ϰ" | "ϱ" | "ϲ" | "ϳ" | "ϴ" | "ϵ" 
    | "϶" | "Ϸ" | "ϸ" | "Ϲ" | "Ϻ" | "ϻ" | "ϼ" | "Ͻ" | "Ͼ" | "Ͽ" | "ἀ" | "ἁ" | "ἂ" | "ἃ" | "ἄ" | "ἅ" | "ἆ" | "ἇ" | "Ἀ" | "Ἁ" 
    | "Ἂ" | "Ἃ" | "Ἄ" | "Ἅ" | "Ἆ" | "Ἇ" | "ἐ" | "ἑ" | "ἒ" | "ἓ" | "ἔ" | "ἕ" | "἖" | "἗" | "Ἐ" | "Ἑ" | "Ἒ" | "Ἓ" | "Ἔ" | "Ἕ" 
    | "἞" | "἟" | "ἠ" | "ἡ" | "ἢ" | "ἣ" | "ἤ" | "ἥ" | "ἦ" | "ἧ" | "Ἠ" | "Ἡ" | "Ἢ" | "Ἣ" | "Ἤ" | "Ἥ" | "Ἦ" | "Ἧ" | "ἰ" | "ἱ" 
    | "ἲ" | "ἳ" | "ἴ" | "ἵ" | "ἶ" | "ἷ" | "Ἰ" | "Ἱ" | "Ἲ" | "Ἳ" | "Ἴ" | "Ἵ" | "Ἶ" | "Ἷ" | "ὀ" | "ὁ" | "ὂ" | "ὃ" | "ὄ" | "ὅ" 
    | "὆" | "὇" | "Ὀ" | "Ὁ" | "Ὂ" | "Ὃ" | "Ὄ" | "Ὅ" | "὎" | "὏" | "ὐ" | "ὑ" | "ὒ" | "ὓ" | "ὔ" | "ὕ" | "ὖ" | "ὗ" | "὘" | "Ὑ" 
    | "὚" | "Ὓ" | "὜" | "Ὕ" | "὞" | "Ὗ" | "ὠ" | "ὡ" | "ὢ" | "ὣ" | "ὤ" | "ὥ" | "ὦ" | "ὧ" | "Ὠ" | "Ὡ" | "Ὢ" | "Ὣ" | "Ὤ" | "Ὥ" 
    | "Ὦ" | "Ὧ" | "ὰ" | "ά" | "ὲ" | "έ" | "ὴ" | "ή" | "ὶ" | "ί" | "ὸ" | "ό" | "ὺ" | "ύ" | "ὼ" | "ώ" | "὾" | "὿" | "ᾀ" | "ᾁ" 
    | "ᾂ" | "ᾃ" | "ᾄ" | "ᾅ" | "ᾆ" | "ᾇ" | "ᾈ" | "ᾉ" | "ᾊ" | "ᾋ" | "ᾌ" | "ᾍ" | "ᾎ" | "ᾏ" | "ᾐ" | "ᾑ" | "ᾒ" | "ᾓ" | "ᾔ" | "ᾕ" 
    | "ᾖ" | "ᾗ" | "ᾘ" | "ᾙ" | "ᾚ" | "ᾛ" | "ᾜ" | "ᾝ" | "ᾞ" | "ᾟ" | "ᾠ" | "ᾡ" | "ᾢ" | "ᾣ" | "ᾤ" | "ᾥ" | "ᾦ" | "ᾧ" | "ᾨ" | "ᾩ" 
    | "ᾪ" | "ᾫ" | "ᾬ" | "ᾭ" | "ᾮ" | "ᾯ" | "ᾰ" | "ᾱ" | "ᾲ" | "ᾳ" | "ᾴ" | "᾵" | "ᾶ" | "ᾷ" | "Ᾰ" | "Ᾱ" | "Ὰ" | "Ά" | "ᾼ" | "᾽" 
    | "ι" | "᾿" | "῁" | "ῂ" | "ῃ" | "ῄ" | "῅" | "ῆ" | "ῇ" | "Ὲ" | "Έ" | "Ὴ" | "Ή" | "ῌ" | "῍" | "῎" | "῏" | "ῐ" | "ῑ" | "ῒ" 
    | "ΐ" | "῔" | "῕" | "ῖ" | "ῗ" | "Ῐ" | "Ῑ" | "Ὶ" | "Ί" | "῜" | "῝" | "῞" | "῟" | "ῠ" | "ῡ" | "ῢ" | "ΰ" | "ῤ" | "ῥ" | "ῦ" 
    | "ῧ" | "Ῠ" | "Ῡ" | "Ὺ" | "Ύ" | "Ῥ" | "῭" | "΅" | "`" | "῰" | "῱" | "ῲ" | "ῳ" | "ῴ" | "῵" | "ῶ" | "ῷ" | "Ὸ" | "Ό" | "Ὼ" 
    | "Ώ" | "ῼ" | "´" | "῾"

(* Regexes for tokens *)
let int = '-'? digit+
let id = ((alpha) (alpha|digit|'_')*) | greek
let generic_type_param =  ['A'-'Z']

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

(* Lexer Rules
 * To disambiguate prefixes, Ocamllex applies:
 *   1) Longest match
 *   2) Match first rule (hence id is listed after keywords) 
 *)

    rule read_token = parse 
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "{" { LBRACE }
    | "}" { RBRACE }
    | "," { COMMA }
    | "." { DOT }
    | ":" { COLON }
    | ";" { SEMICOLON }
    | "=" { EQUAL }
    | "+" { PLUS }
    | "-" { MINUS }
    | "*" { MULT } 
    | "/" { DIV }
    | "%" { MOD }
    | "^" { POWER }
    | "-/" { ROOT }
    | "<" { LANGLE }
    | ">" { RANGLE }
    | "<<" { DLANGLE }
    | ">>" { DRANGLE }
    | "<-" { LARROW }
    | "->" { RARROW }
    | "<=" { FATLARROW }
    | "=>" { FATRARROW }
    | "!<" { RETURN }
    | "&"  { AND }
    | "|"  { OR }
    | "&&" { DAND }
    | "||" { DOR }
    | "::" { TAKE }
    | "!" { EXCLAMATION_MARK }
    | "~" { TILDE }

    | "i8" { T_I8 }
    | "i16" { T_I16 }
    | "i32" { T_I32 }
    | "i64" { T_I64 }
    | "i128" { T_I128 }
    | "u8" { T_U8 }
    | "u16" { T_U16 }
    | "u32" { T_U32 }
    | "u64" { T_U64 }
    | "u128" { T_U128 }
    | "bool" { T_BOOL }
    | "empty" { T_EMPTY }

    | "include" { INCLUDE }
    | "fn" { FUNCTION }
    | "let" { LET }
    | "const" { CONST }
    | "mut" { MUT }
    | "in" { IN }
    | "struct" { STRUCT }
    | "union" { UNION }
    | "enum" { ENUM }
    | "match" { MATCH }
    | "*" { BORROWED }
    | "&" { REF }

    | "true" { TRUE }
    | "false" { FALSE }
    | "while" { WHILE }
    | "if" { IF }
    | "else" { ELSE }
    | "for" { FOR }
    | generic_type_param { GENERIC (Lexing.lexeme lexbuf).[0] }
    | whitespace { read_token lexbuf }
    | "//" { read_single_line_comment lexbuf }
    | "/*" { read_multi_line_comment lexbuf } 
    | int { INT (int_of_string (Lexing.lexeme lexbuf))}
    | character { CHAR (Lexing.lexeme lexbuf).[0] }
    | id { ID (Lexing.lexeme lexbuf) }
    | '"' { read_string (Buffer.create 17) lexbuf }
    | newline { next_line lexbuf; read_token lexbuf }
    | eof { EOF }
    | _ {
        raise (SyntaxError ("Illegal character: " ^ Lexing.lexeme lexbuf)) 
    }

    and read_single_line_comment = parse
    | newline { next_line lexbuf; read_token lexbuf } 
    | eof { EOF }
    | _ { read_single_line_comment lexbuf } 

    and read_multi_line_comment = parse
    | "*/" { read_token lexbuf } 
    | newline { next_line lexbuf; read_multi_line_comment lexbuf } 
    | eof { raise (SyntaxError ("Unexpected EOF - please terminate your comment.")) }
    | _ { read_multi_line_comment lexbuf } 

    and read_string buf = parse
    | '"'       { STRING (Buffer.contents buf) }
    | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
    | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
    | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
    | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
    | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
    | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
    | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
    | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
        read_string buf lexbuf
    }
    | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
    | eof { raise (SyntaxError ("String is not terminated")) }

