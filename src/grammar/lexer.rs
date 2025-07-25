use std::{
    collections::HashMap,
    fmt::{Debug, Display},
};

use crate::common::{OpacaError, try_index};

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    Eof,
    Bool(bool),
    Int(i32),
    Float(f32),
    String(String),
    Ident(String),
    StatementEnding,
    Colon,
    Assign,
    ArrowLeft,
    ArrowRight,
    Add,
    Concat,
    Subtract,
    Multiply,
    Divide,
    Modulus,
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    And,
    Or,
    Dot,
    Module,
    Use,
    Exports,
    Fun,
    Is,
    Do,
    End,
    Return,
    Let,
    If,
    Elsif,
    Else,
    Comma,
    ParenLeft,
    ParenRight,
    BracketLeft,
    BracketRight,
    BraceLeft,
    BraceRight,
}

impl TokenType {
    pub fn is_ending(&self) -> bool {
        *self == Self::Eof || *self == Self::StatementEnding || *self == Self::End
    }

    pub fn get_string(&self) -> &String {
        match self {
            Self::Ident(s) => s,
            Self::String(s) => s,
            _ => panic!("TokenType::get_string expects TokenType::Ident or TokenType::String"),
        }
    }

    pub fn is_pair_of(&self, other: &TokenType) -> bool {
        (*self == Self::ParenLeft && *other == Self::ParenRight)
            || (*self == Self::BracketLeft && *other == Self::BracketRight)
            || (*self == Self::BraceLeft && *other == Self::BraceRight)
    }
}

impl Into<String> for TokenType {
    fn into(self) -> String {
        let str = match self {
            Self::Bool(v) => v.to_string(),
            Self::Int(v) => v.to_string(),
            Self::Float(v) => v.to_string(),
            Self::String(v) => format!("\"{}\"", v),
            Self::Ident(v) => v,
            _ => match self {
                Self::Bool(_) => unreachable!(),
                Self::Int(_) => unreachable!(),
                Self::Float(_) => unreachable!(),
                Self::String(_) => unreachable!(),
                Self::Ident(_) => unreachable!(),
                Self::Eof => "Eof",
                Self::StatementEnding => ";",
                Self::Colon => ":",
                Self::ArrowLeft => "<-",
                Self::ArrowRight => "->",
                Self::Assign => "=",
                Self::Add => "+",
                Self::Concat => "++",
                Self::Subtract => "-",
                Self::Multiply => "*",
                Self::Divide => "/",
                Self::Modulus => "%",
                Self::Equals => "==",
                Self::NotEquals => "!=",
                Self::LessThan => "<",
                Self::GreaterThan => ">",
                Self::And => "and",
                Self::Or => "or",
                Self::Dot => ".",
                Self::Module => "module",
                Self::Use => "use",
                Self::Exports => "exports",
                Self::Fun => "fun",
                Self::Is => "is",
                Self::Do => "do",
                Self::End => "end",
                Self::Return => "return",
                Self::Let => "let",
                Self::If => "if",
                Self::Elsif => "elsif",
                Self::Else => "else",
                Self::Comma => ",",
                Self::ParenLeft => "(",
                Self::ParenRight => ")",
                Self::BracketLeft => "[",
                Self::BracketRight => "]",
                Self::BraceLeft => "{",
                Self::BraceRight => "}",
            }
            .to_string(),
        };

        str
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let sym: String = self.clone().into();
        write!(f, "{}", sym)
    }
}

#[derive(Clone)]
pub struct Token {
    typ: TokenType,
    col: usize,
    ln: usize,
    file: String,
}

impl Token {
    pub fn new(typ: TokenType, col: usize, ln: usize, file: String) -> Token {
        Token {
            typ: typ,
            col: col,
            ln: ln,
            file: file,
        }
    }

    pub fn get_typ(&self) -> &TokenType {
        &self.typ
    }

    pub fn err(&self) -> String {
        format!("{}:{}:{}", self.file, self.ln, self.col)
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "<{:?}, {}, {}, '{}'>",
            self.typ, self.col, self.ln, self.file
        )
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (self as &dyn Debug).fmt(f)
    }
}

impl PartialEq<TokenType> for Token {
    fn eq(&self, other: &TokenType) -> bool {
        self.typ == *other
    }

    fn ne(&self, other: &TokenType) -> bool {
        !(self == other)
    }
}

#[macro_export]
macro_rules! token_err {
    ($tok:expr, $fmt:expr, $($msg:tt)*) => {
        format!("{}\n{}", $tok.err(), format!($fmt, $($msg)*))
    };
}

#[macro_export]
macro_rules! token_opacaerr {
    ($tok:expr, $fmt:expr, $($msg:tt)*) => {
        OpacaError::from(crate::token_err!($tok, $fmt, $($msg)*))
    };
}

fn create_chartoks() -> HashMap<char, TokenType> {
    HashMap::from([
        (';', TokenType::StatementEnding),
        (':', TokenType::Colon),
        ('.', TokenType::Dot),
        (',', TokenType::Comma),
        ('+', TokenType::Add),
        ('-', TokenType::Subtract),
        ('*', TokenType::Multiply),
        ('/', TokenType::Divide),
        ('%', TokenType::Modulus),
        ('=', TokenType::Assign),
        ('<', TokenType::LessThan),
        ('>', TokenType::GreaterThan),
        ('(', TokenType::ParenLeft),
        (')', TokenType::ParenRight),
        ('[', TokenType::BracketLeft),
        (']', TokenType::BracketRight),
        ('{', TokenType::BraceLeft),
        ('}', TokenType::BraceRight),
    ])
}

fn create_duotoks() -> HashMap<&'static str, TokenType> {
    HashMap::from([
        ("==", TokenType::Equals),
        ("!=", TokenType::NotEquals),
        ("++", TokenType::Concat),
        ("<-", TokenType::ArrowLeft),
        ("->", TokenType::ArrowRight),
    ])
}

pub struct Lexer {
    file: String,
    text: Vec<char>,
    idx: usize,
    col: usize,
    ln: usize,
    chartoks: HashMap<char, TokenType>,
    duotoks: HashMap<&'static str, TokenType>,
}

impl Lexer {
    pub fn new(file: String, text: &String) -> Lexer {
        Lexer {
            file: file,
            text: text.chars().collect(),
            idx: 0,
            col: 1,
            ln: 1,
            chartoks: create_chartoks(),
            duotoks: create_duotoks(),
        }
    }

    fn adv(&mut self) {
        self.idx += 1;
        self.col += 1;

        if try_index(&self.text, self.idx).is_some_and(|ch| *ch == '\n') {
            self.ln += 1;
            self.col = 1;
        }
    }

    fn new_token(&self, typ: TokenType, col: usize, ln: usize) -> Token {
        Token::new(typ, col, ln, self.file.clone())
    }

    fn is_num(&self, idx: usize) -> bool {
        try_index(&self.text, idx).is_some_and(|ch| ch.is_ascii_digit() || *ch == '.')
    }

    fn is_ident(&self) -> bool {
        try_index(&self.text, self.idx).is_some_and(|ch| ch.is_ascii_alphanumeric() || *ch == '_')
    }

    fn collect_num(&mut self) -> Token {
        let startcol = self.col;
        let startln = self.ln;

        let mut str = String::new();

        let mut dot = false;

        while self.is_num(self.idx) {
            if self.text[self.idx] == '.' {
                if dot {
                    break;
                }

                dot = true;
            }

            str.push(self.text[self.idx]);
            self.adv();
        }

        let e_msg = format!("somehow got the invalid string '{}' in collect_num", str);

        let typ = if dot {
            TokenType::Float(str.parse().expect(e_msg.as_str()))
        } else {
            TokenType::Int(str.parse().expect(e_msg.as_str()))
        };

        Token::new(typ, startcol, startln, self.file.clone())
    }

    fn collect_ident(&mut self) -> Token {
        let startcol = self.col;
        let startln = self.ln;

        let mut str = String::new();

        while self.is_ident() {
            str.push(self.text[self.idx]);
            self.adv();
        }

        if let Some(ch) = try_index(&self.text, self.idx) {
            if *ch == '\'' || *ch == '"' {
                str.push(*ch);
                self.adv();
            }
        }

        let tt = match str.as_str() {
            "true" => TokenType::Bool(true),
            "false" => TokenType::Bool(false),
            "module" => TokenType::Module,
            "is" => TokenType::Is,
            "fun" => TokenType::Fun,
            "use" => TokenType::Use,
            "exports" => TokenType::Exports,
            "do" => TokenType::Do,
            "end" => TokenType::End,
            "if" => TokenType::If,
            "elsif" => TokenType::Elsif,
            "else" => TokenType::Else,
            "return" => TokenType::Return,
            "let" => TokenType::Let,
            "and" => TokenType::And,
            "or" => TokenType::Or,
            _ => TokenType::Ident(str),
        };

        self.new_token(tt, startcol, startln)
    }

    fn collect_string(&mut self) -> Result<Token, OpacaError> {
        let startcol = self.col;
        let startln = self.ln;

        let mut str = String::new();
        let mut escaped = false;

        self.adv();

        while let Some(ch) = try_index(&self.text, self.idx) {
            if escaped {
                str.push(match *ch {
                    '\\' | '"' | '\'' => *ch,
                    'n' => '\n',
                    't' => '\t',
                    'a' => '\u{7}',
                    '0' => '\u{0}',
                    _ => {
                        return Err(token_opacaerr!(
                            Token::new(TokenType::Eof, self.col, self.ln, self.file.clone()),
                            "invalid escape character '{}'",
                            ch
                        ));
                    }
                });

                escaped = false;
            } else if *ch == '\\' {
                escaped = true;
            } else if *ch == '"' {
                break;
            } else {
                str.push(*ch);
            }

            self.adv();
        }

        if !try_index(&self.text, self.idx).is_some_and(|ch| *ch == '"') {
            return Err(token_opacaerr!(
                self.new_token(TokenType::Eof, self.col, self.ln),
                "unterminated string literal",
            ));
        }

        self.adv();

        Ok(self.new_token(TokenType::String(str), startcol, startln))
    }

    fn skip_singleline_comment(&mut self) {
        while try_index(&self.text, self.idx).is_some_and(|ch| *ch != '\n') {
            self.adv()
        }
    }

    fn skip_multiline_comment(&mut self) -> Result<(), OpacaError> {
        let startcol = self.col;
        let startln = self.ln;

        while let Some(ch) = try_index(&self.text, self.idx) {
            if try_index(&self.text, self.idx + 1).is_some_and(|subch| *ch == ']' && *subch == '#')
            {
                self.adv();
                self.adv();
                return Ok(());
            }

            self.adv()
        }

        Err(token_opacaerr!(
            self.new_token(TokenType::Eof, startcol, startln),
            "unterminated multi-line comment",
        ))
    }

    fn check_duotok(&self, ch: &char) -> Option<&TokenType> {
        if let Some(peeked) = try_index(&self.text, self.idx + 1) {
            let mut str = String::new();
            str.push(*ch);
            str.push(*peeked);

            self.duotoks.get(str.as_str())
        } else {
            None
        }
    }

    pub fn lex(&mut self) -> Result<Vec<Token>, OpacaError> {
        let mut tokens: Vec<Token> = Vec::new();

        let text_clone = self.text.clone();

        while let Some(ch) = try_index(&text_clone, self.idx) {
            if ch.is_whitespace() {
                self.adv();
            } else if *ch == '#' {
                if try_index(&text_clone, self.idx + 1).is_some_and(|subch| *subch == '[') {
                    self.skip_multiline_comment()?
                } else {
                    self.skip_singleline_comment()
                }
            } else if self.is_num(self.idx) && *ch != '.' && *ch != '_' {
                tokens.push(self.collect_num())
            } else if self.is_ident() {
                tokens.push(self.collect_ident())
            } else if *ch == '"' {
                tokens.push(self.collect_string()?)
            } else if let Some(duott) = self.check_duotok(ch) {
                tokens.push(self.new_token(duott.clone(), self.col, self.ln));
                self.adv();
                self.adv()
            } else if let Some(tt) = self.chartoks.get(ch) {
                tokens.push(self.new_token(tt.clone(), self.col, self.ln));

                self.adv()
            } else {
                return Err(token_opacaerr!(
                    self.new_token(TokenType::Eof, self.col, self.ln),
                    "unexpected character '{}'",
                    ch
                ));
            }
        }

        tokens.push(self.new_token(TokenType::Eof, self.col, self.ln));

        Ok(tokens)
    }
}
