use std::fmt::Display;

use crate::{
    common::{OpacaError, variant_eq},
    grammar::lexer::{Token, TokenType},
    token_anaerr,
};

pub enum FunctionBody {
    Block(Vec<Node>),
    Expr(Box<Node>),
}

pub enum Node {
    Module {
        name: Token,
        contents: Vec<Node>,
    },
    Use {
        start: Token,
        path: Vec<Token>,
    },
    Exports {
        start: Token,
        symbols: Vec<Token>,
    },
    Function {
        start: Token,
        name: Token,
        params: Vec<Token>,
        body: FunctionBody,
    },
    Let {
        start: Token,
        name: Token,
        expr: Box<Node>,
    },
    Int(Token),
    Float(Token),
    Ident(Token),
    BinaryExpr {
        left: Box<Node>,
        op: Token,
        right: Box<Node>,
    },
    Tuple(Vec<Node>),
    FunctionCall {
        callee: Box<Node>,
        args: Box<Node>,
    },
    Return(Box<Node>),
}

impl Node {
    pub fn format(&self, indent: usize) -> String {
        let str = match self {
            Self::Module { name, contents } => {
                let contentsf = contents
                    .iter()
                    .map(|n| n.format(indent + 1))
                    .collect::<Vec<String>>()
                    .join(",\n");

                format!(
                    "Module {{\n name: {},\n contents:\n {},\n}}",
                    name, contentsf
                )
            }

            Self::Use { start, path } => {
                let pathf = path
                    .iter()
                    .map(|n| n.to_string())
                    .collect::<Vec<String>>()
                    .join(",\n  ");

                format!("Use {{\n start: {}\n path:\n  {},\n }}", start, pathf)
            }

            Self::Exports { start, symbols } => {
                let symbolsf = symbols
                    .iter()
                    .map(|n| n.to_string())
                    .collect::<Vec<String>>()
                    .join(",\n  ");

                format!(
                    "Exports {{\n start: {}\n symbols:\n  {},\n }}",
                    start, symbolsf
                )
            }

            Self::Function {
                start,
                name,
                params,
                body,
            } => {
                let bodyf = match body {
                    FunctionBody::Block(block) => block
                        .iter()
                        .map(|n| n.format(indent + 1))
                        .collect::<Vec<String>>()
                        .join(",\n"),

                    FunctionBody::Expr(expr) => expr.format(indent + 1),
                };

                let paramsf = params
                    .iter()
                    .map(|n| n.to_string())
                    .collect::<Vec<String>>()
                    .join(",\n  ");

                format!(
                    "Function {{\n start: {},\n name: {},\n params:\n  {},\n contents:\n{},\n}}",
                    start, name, paramsf, bodyf
                )
            }

            Self::Let { start, name, expr } => {
                format!(
                    "Let {{\n start: {},\n name: {},\n expr: {},\n}}",
                    start, name, expr,
                )
            }

            Self::Int(t) => format!("Int: {}", t),

            Self::Float(t) => format!("Float: {}", t),

            Self::Ident(t) => format!("Ident: {}", t),

            Self::Tuple(contents) => {
                let contentsf = contents
                    .iter()
                    .map(|n| n.format(indent + 1))
                    .collect::<Vec<String>>()
                    .join(",\n");

                format!("Tuple {{\n contents:\n {},\n}}", contentsf)
            }

            Self::BinaryExpr { left, op, right } => {
                format!(
                    "BinaryExpr {{\n left:\n  {},\n op: {},\n right:\n  {},\n}}",
                    left.format(indent + 1),
                    op,
                    right.format(indent + 1),
                )
            }

            Self::FunctionCall { callee, args } => format!(
                "FunctionCall {{\n callee:\n  {},\n args:\n  {},\n}}",
                callee.format(indent + 1),
                args.format(indent + 1),
            ),

            Self::Return(expr) => format!("Return: {{\n expr:\n  {}\n}}", expr.format(indent + 1)),
        };

        let mut pad = String::new();

        for _ in 0..indent {
            pad.push(' ');
        }

        str.lines()
            .map(|line| pad.clone() + line)
            .collect::<Vec<String>>()
            .join("\n")
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.format(0))
    }
}

type ParserResult = Result<(), OpacaError>;
type ParserNodeResult = Result<Node, OpacaError>;

pub struct Parser {
    tokens: Vec<Token>,
    idx: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens: tokens,
            idx: 0,
        }
    }

    fn cur(&self) -> &Token {
        return &self.tokens[self.idx];
    }

    fn next(&mut self) -> &Token {
        let t = &self.tokens[self.idx];
        self.idx += 1;
        return t;
    }

    fn eat(&mut self) {
        _ = self.next()
    }

    fn expect_t(&self, t: &Token, tt: TokenType) -> ParserResult {
        if variant_eq(t.get_typ(), &tt) {
            Ok(())
        } else {
            Err(token_anaerr!(
                t,
                "expected token '{}', but found '{}' instead",
                tt,
                t.get_typ(),
            ))
        }
    }

    fn expect(&self, tt: TokenType) -> ParserResult {
        self.expect_t(self.cur(), tt)
    }

    fn expect_and_eat(&mut self, tt: TokenType) -> ParserResult {
        self.expect(tt)?;
        self.eat();
        Ok(())
    }

    fn expect_and_get(&mut self, tt: TokenType) -> Result<&Token, OpacaError> {
        self.expect(tt)?;
        Ok(self.next())
    }

    pub fn parse(&mut self) -> Result<Vec<Node>, OpacaError> {
        let mut modules: Vec<Node> = Vec::new();

        loop {
            if *self.cur().get_typ() == TokenType::Eof {
                break;
            }

            modules.push(self.parse_module()?);
        }

        Ok(modules)
    }

    fn parse_module(&mut self) -> ParserNodeResult {
        let mut nodes: Vec<Node> = Vec::new();

        let module_start = self.expect_and_get(TokenType::Module)?.clone();

        let name = self
            .expect_and_get(TokenType::Ident(String::new()))?
            .clone();

        self.expect_and_eat(TokenType::Is)?;

        loop {
            let tok = self.next().clone();

            match tok.get_typ() {
                TokenType::Use => {
                    let node = self.parse_use(&tok)?;

                    self.expect_and_eat(TokenType::StatementEnding)?;

                    nodes.push(node);
                }

                TokenType::Exports => {
                    let node = self.parse_exports(&tok)?;

                    self.expect_and_eat(TokenType::StatementEnding)?;

                    nodes.push(node);
                }

                TokenType::Fun => {
                    let node = self.parse_fun(&tok)?;

                    self.expect_and_eat(TokenType::StatementEnding)?;

                    nodes.push(node);
                }

                TokenType::End => break,

                TokenType::Module => {
                    return Err(token_anaerr!(tok, "module nesting is not supported",));
                }

                TokenType::Let => {
                    let node = self.parse_let(&tok)?;

                    self.expect_and_eat(TokenType::StatementEnding)?;

                    nodes.push(node);
                }

                TokenType::Eof => {
                    return Err(token_anaerr!(
                        module_start,
                        "expected 'end' to terminate module definition",
                    ));
                }

                _ => {
                    return Err(token_anaerr!(
                        tok,
                        "unexpected token '{}' in module definition",
                        tok.get_typ()
                    ));
                }
            }
        }

        Ok(Node::Module {
            name: name.clone(),
            contents: nodes,
        })
    }

    fn parse_use(&mut self, start: &Token) -> ParserNodeResult {
        let mut path: Vec<Token> = Vec::new();

        loop {
            let ident = self.expect_and_get(TokenType::Ident(String::new()))?;

            path.push(ident.clone());

            if self.cur().get_typ().is_ending() {
                break;
            }

            self.expect_and_eat(TokenType::Dot)?
        }

        Ok(Node::Use {
            start: start.clone(),
            path: path,
        })
    }

    fn parse_exports(&mut self, start: &Token) -> ParserNodeResult {
        let mut symbols: Vec<Token> = Vec::new();

        loop {
            let ident = self.expect_and_get(TokenType::Ident(String::new()))?;

            symbols.push(ident.clone());

            if self.cur().get_typ().is_ending() {
                break;
            }

            self.expect_and_eat(TokenType::Comma)?
        }

        Ok(Node::Exports {
            start: start.clone(),
            symbols: symbols,
        })
    }

    fn parse_fun(&mut self, start: &Token) -> ParserNodeResult {
        let name = self
            .expect_and_get(TokenType::Ident(String::new()))?
            .clone();

        self.expect_and_eat(TokenType::ParenLeft)?;

        let params = self.parse_fun_params()?;

        self.expect_and_eat(TokenType::ParenRight)?;

        self.expect_and_eat(TokenType::Assign)?;

        let tok = self.next().clone();

        match tok.get_typ() {
            TokenType::Do => {
                let nodes = self.parse_fun_block(&tok)?;

                self.expect_and_eat(TokenType::End)?;

                Ok(Node::Function {
                    start: start.clone(),
                    name: name.clone(),
                    params: params,
                    body: FunctionBody::Block(nodes),
                })
            }

            TokenType::Eof | TokenType::StatementEnding => Err(token_anaerr!(
                tok,
                "expected expression or do .. end in function definition",
            )),

            _ => {
                self.idx -= 1;

                let expr = self.parse_expr(0)?;

                Ok(Node::Function {
                    start: start.clone(),
                    name: name.clone(),
                    params: params,
                    body: FunctionBody::Expr(Box::new(expr)),
                })
            }
        }
    }

    fn parse_fun_params(&mut self) -> Result<Vec<Token>, OpacaError> {
        let mut args: Vec<Token> = Vec::new();

        if self.cur().get_typ().is_ending() || *self.cur().get_typ() == TokenType::ParenRight {
            return Ok(args);
        }

        loop {
            let ident = self.expect_and_get(TokenType::Ident(String::new()))?;

            args.push(ident.clone());

            if self.cur().get_typ().is_ending() || *self.cur().get_typ() == TokenType::ParenRight {
                break;
            }

            self.expect_and_eat(TokenType::Comma)?
        }

        Ok(args)
    }

    fn parse_fun_block(&mut self, start: &Token) -> Result<Vec<Node>, OpacaError> {
        let mut nodes: Vec<Node> = Vec::new();

        if self.cur().get_typ().is_ending() {
            return Ok(nodes);
        }

        loop {
            let tok = self.next().clone();

            match tok.get_typ() {
                TokenType::Let => {
                    let node = self.parse_let(&tok)?;

                    self.expect_and_eat(TokenType::StatementEnding)?;

                    nodes.push(node);
                }

                TokenType::Ident(_) => {
                    self.idx -= 1;

                    let expr = self.parse_expr(0)?;

                    if *self.cur().get_typ() != TokenType::End {
                        self.expect_and_eat(TokenType::StatementEnding)?;
                    }

                    nodes.push(expr);
                }

                TokenType::End => break,

                TokenType::Eof => {
                    return Err(token_anaerr!(
                        start,
                        "expected 'end' to terminate function body",
                    ));
                }

                _ => {
                    return Err(token_anaerr!(
                        tok,
                        "unexpected token '{}' in function body",
                        tok.get_typ()
                    ));
                }
            }

            if self.cur().get_typ().is_ending() {
                break;
            }
        }

        Ok(nodes)
    }

    fn parse_let(&mut self, start: &Token) -> ParserNodeResult {
        let name = self
            .expect_and_get(TokenType::Ident(String::new()))?
            .clone();

        self.expect_and_eat(TokenType::Assign)?;

        let expr = self.parse_expr(0)?;

        Ok(Node::Let {
            start: start.clone(),
            name: name,
            expr: Box::new(expr),
        })
    }

    fn parse_expr(&mut self, min_bp: u8) -> ParserNodeResult {
        let lht = self.next().clone();

        println!("lht: {}", lht);

        let mut lhs = match lht.get_typ() {
            TokenType::Int(_) => Node::Int(lht),
            TokenType::Float(_) => Node::Float(lht),
            TokenType::Ident(_) => Node::Ident(lht),
            TokenType::ParenLeft => {
                let expr = self.parse_expr(0)?;

                self.expect_and_eat(TokenType::ParenRight)?;

                expr
            }
            _ => {
                return Err(token_anaerr!(
                    lht,
                    "unexpected token '{}' in expression",
                    lht.get_typ()
                ));
            }
        };

        loop {
            let opt = self.next().clone();

            let op = match opt.get_typ() {
                TokenType::Eof | TokenType::StatementEnding => break,

                TokenType::And
                | TokenType::Or
                | TokenType::Equals
                | TokenType::NotEquals
                | TokenType::LessThan
                | TokenType::GreaterThan
                | TokenType::Dot
                | TokenType::Multiply
                | TokenType::Divide
                | TokenType::Modulus
                | TokenType::Add
                | TokenType::Subtract => opt,

                // non-operators
                TokenType::ParenRight => opt,

                TokenType::ParenLeft => {
                    let tuple = self.parse_tuple()?;

                    println!("after tuple: {}", self.cur());

                    // roll back index if the tuple is non-empty
                    // this exception is needed because
                    // the expression parsing consumes the closing ')'
                    if let Node::Tuple(ref items) = tuple {
                        if items.len() > 0 {
                            self.idx -= 2;
                        }
                    }

                    self.expect_and_eat(TokenType::ParenRight)?;

                    println!("after tuple expect: {}", self.cur());

                    lhs = Node::FunctionCall {
                        callee: Box::new(lhs),
                        args: Box::new(tuple),
                    };

                    break;
                }

                _ => {
                    return Err(token_anaerr!(
                        opt,
                        "unexpected operator token '{}' in expression",
                        opt.get_typ()
                    ));
                }
            };

            if let Some((l_bp, r_bp)) = infix_binding_power(op.get_typ()) {
                if l_bp < min_bp {
                    self.idx -= 1;
                    break;
                }

                let rhs = self.parse_expr(r_bp)?;

                lhs = Node::BinaryExpr {
                    left: Box::new(lhs),
                    op: op,
                    right: Box::new(rhs),
                };

                continue;
            }

            break;
        }

        Ok(lhs)
    }

    fn parse_tuple(&mut self) -> ParserNodeResult {
        let mut items: Vec<Node> = Vec::new();

        if self.cur().get_typ().is_ending() || *self.cur().get_typ() == TokenType::ParenRight {
            return Ok(Node::Tuple(items));
        }

        loop {
            let expr = self.parse_expr(0)?;

            items.push(expr);

            if self.cur().get_typ().is_ending() || *self.cur().get_typ() == TokenType::ParenRight {
                break;
            }

            self.expect_and_eat(TokenType::Comma)?
        }

        Ok(Node::Tuple(items))
    }
}

fn infix_binding_power(op: &TokenType) -> Option<(u8, u8)> {
    match op {
        TokenType::Add | TokenType::Subtract => Some((1, 2)),
        TokenType::Multiply | TokenType::Divide | TokenType::Modulus => Some((3, 4)),
        TokenType::Dot => Some((5, 6)),
        TokenType::Equals | TokenType::NotEquals | TokenType::LessThan | TokenType::GreaterThan => {
            Some((7, 8))
        }
        TokenType::And | TokenType::Or => Some((9, 10)),
        _ => None,
    }
}
