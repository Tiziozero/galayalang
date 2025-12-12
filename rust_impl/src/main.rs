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
                        "while" | "for" | "if" | "do" | "return" | "else" => {
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
                '+' | '-' | '*' | '/' | '=' | '<' | '>' | '!' | '&' | '|' => {
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
                ';' | ',' | '(' | ')' | '{' | '}' | '[' | ']' => {
                    self.tokens.push(Token {
                        ttype:TokenType::Punctuation(c),
                        column, line });
                    chars.next();
                    column += 1;
                },

                _=> panic!("invalid token") // shoudn't happen

            }
        }
    }
    
}


struct Parser {

}

fn main() -> std::io::Result<()> {
    let mut f = File::open("../test.new_c")?;
    let mut contents = String::new();
    f.read_to_string(&mut contents)?;

    let mut l = Lexer::new();
    l.lexe(contents);

    for token in l.tokens {
        println!("{:?}", token);
    }
    Ok(())
}
