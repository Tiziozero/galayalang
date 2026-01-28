#[allow(unused)]
use std::fs::File;
use std::io::prelude::*;

#[derive(Clone, Debug, PartialEq)]
enum TokenType {
    Number(String),
    Identifier(String),
    Keyword(String),
    Punctuation(char),
    Operator(String),
}

#[derive(Clone, Debug)]
struct Token {
    ttype: TokenType,
    line: usize,
    column: usize,
}
struct Lexer {
    tokens: Vec<Token>,
}

impl Lexer {
    pub fn new() -> Self {
        Lexer{ tokens: Vec::new() }
    }
    pub fn lexe(&mut self, s:String) {
        let mut line = 1;
        let mut column = 1;
        let mut chars = s.chars().peekable();

        while let Some(&c) = chars.peek() {
            match c {
                ' ' | '\t' => {
                    chars.next();
                    column += 1;
                },
                '\n' => {
                    chars.next();
                    line += 1; 
                    column = 1;
                },
                '0'..='9' => {
                    let mut number = String::new();
                    while let Some(&d) = chars.peek() {
                        if d.is_ascii_digit() {
                            number.push(d);
                            chars.next();
                            column += 1;
                        } else { 
                            break;
                        }
                    }
                    self.tokens.push( Token {
                        ttype: TokenType::Number(number),
                        line: line,
                        column: column,

                    });
                },
                'a'..='z' | 'A'..='Z' | '_' => {
                    let mut ident = String::new();
                    while let Some(&c) = chars.peek() {
                        if c.is_alphanumeric() || c == '_' {
                            ident.push(c);
                            chars.next();
                            column += 1;
                        } else {
                            break;
                        }
                    }
                    match ident.as_str() {
                        "let" | "fn" | "struct" | "enum" |"while" | "for" | "if" | "do" | "return" | "else" => {
                            self.tokens.push(Token {
                                ttype:TokenType::Keyword(ident),
                                column, line });
                        },
                        _=> {
                            self.tokens.push(Token {
                                ttype:TokenType::Identifier(ident),
                                column, line });
                        }
                    }
                },
                '+' | '-' | '*' | '/' | '=' | '<' | '>' | '!' | '&' | '|' | '~' => {
                    let mut op = String::new();
                    op.push(c);
                    chars.next();

                    if let Some(&next) = chars.peek() {
                        let combined = format!("{}{}", op, next);
                        match combined.as_str() {
                            "==" | "<=" | ">=" | "&&" | "||" => {
                                op.push(c);
                                chars.next();
                                column += 1;
                            },
                            _ => {},
                        }
                    }
                    self.tokens.push(Token {
                        ttype:TokenType::Operator(op),
                        column, line });
                },
                ';' | ',' | '(' | ')' | '{' | '}' | '[' | ']' | ':' => {
                    self.tokens.push(Token {
                        ttype:TokenType::Punctuation(c),
                        column, line });
                    chars.next();
                    column += 1;
                },

                _ => {
                    println!("unknown {}", c);
                    return ;
                    // panic!("invalid token")
                }// shoudn't happen

            }
        }
    }
    
}

struct AST {
    progrman: Vec<Stmt>,
}
impl AST {
    fn new() -> Self {
        AST { progrman: vec![] }
    }
}
struct Parser {
    tokens: Vec<Token>,
    pos: usize, // points to next token
}

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub line: usize,
    pub column: usize,
}
impl ParseError {
    fn from_token(t: &Token) -> Self {
        ParseError { message: String::from(
            format!("Unexpected {:?}", t)
        ), line: t.line, column: t.column }
    }
    fn new(message: String, line: usize, column: usize) -> Self {
        ParseError { message, line, column }
    }
}
impl Parser {
    fn new(tokens: Vec<Token>) ->Self {
        Parser{ tokens, pos:0 }
    }
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }
    fn peek_next(&self) -> Option<&Token> {
        self.tokens.get(self.pos + 1)
    }
    fn consume(&mut self) -> Option<&Token> {
        let token = self.tokens.get(self.pos)?;
        self.pos += 1;
        Some(token)
    }
    /*
    * rules
    *   statement =     "let" identifier { "=" expression } ";" |
    *                   "fn" identifier "(" { args * } ")" ";" |
    *                   "fn" identifier "(" { args * } ")" "{" block "}" |
    *                   identifier "=" expression ";"
    *   identifier =          a-zA-Z0-9... identifier bs
    *   expression =    term { "+" | "-" term } ";"
    *   term =          number | identifier
    *                   
    *
    * */
    fn parse(&mut self) -> Result<AST, ParseError> {
        let mut ast = AST::new();

        while self.peek().is_some() {
            let stmt = self.parse_tlstatement()?;
            ast.progrman.push(stmt);
        }
        Ok(ast)
    }
    fn parse_tlstatement(&mut self) -> Result<Stmt, ParseError> {
        if let Some(t) = self.consume() {
            match &t.ttype {
                TokenType::Keyword(kw) if kw == "let" => self.parse_let(),
                TokenType::Keyword(kw) if kw == "fn" => self.parse_fn(),
                TokenType::Keyword(kw) if kw == "struct"=>self.parse_struct(),
                TokenType::Keyword(kw) if kw == "enum" => self.parse_enum(),
                _=> Err(ParseError::new(String::from(
                    format!("Unexpected {:?}", t.ttype)), t.line,t.column))
            }
        } else {
            Err(ParseError::new(String::from("No token to parse"), 0, 0))
        }
    }
    // (maybe later) let name = expression;
    // let name: type;
    // let name: type = expression;
    fn parse_let(&mut self) -> Result<Stmt,ParseError> {
        if let TokenType::Identifier(name) = &self.consume().unwrap().ttype {
            println!("New variable: {}", name);

            Ok(Stmt::Let(VarDec{name:name.clone()}))
        } else {
            Err(ParseError::new(String::from("Idk"), 0, 0))
        }
    }
    fn parse_fn(&mut self) -> Result<Stmt,ParseError> {
        panic!("todo");
    }
    fn parse_struct(&mut self) -> Result<Stmt,ParseError> {
        panic!("todo");
    }
    fn parse_enum(&mut self) -> Result<Stmt,ParseError> {
        panic!("todo");
    }
    fn parse_statement(&mut self) -> Result<Stmt,ParseError> {
        panic!("todo");
    }
}

enum Stmt {
    Let(VarDec),
    Fn(),
    Struct(),
}
struct VarDec {
    name: String,

}


fn main() -> std::io::Result<()> {
    let mut f = File::open("test.gala")?;
    let mut contents = String::new();
    f.read_to_string(&mut contents)?;
    println!("{}", contents);
    fn helper(contents: &String) {
        println!("{}", contents);
    }
    let _a: &u32;
    helper(&contents);
    let mut l = Lexer::new();
    l.lexe(contents);

    for token in &l.tokens {
        println!("{:?}", token);
    }
    let mut p = Parser::new(l.tokens);
    let _ = p.parse().unwrap();


    Ok(())
}
