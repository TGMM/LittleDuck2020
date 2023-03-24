#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Id(Id),
    Str(String),
    Num(VarValue),
    Program,
    LParen,
    RParen,
    LBracket,
    RBracket,
    Add,
    Sub,
    Div,
    Mul,
    Gt,
    Lt,
    LtGt,
    Eq,
    Comma,
    Float,
    Int,
    TypeSep,
    StmtEnd,
    Print,
    If,
    Else,
    Var,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Id(pub String);

pub struct Program {
    pub id: Id,
    pub vars: Vec<Var>,
    pub block: Block,
}

#[derive(Clone, Copy)]
pub enum VarType {
    Float,
    Int,
}

#[derive(Clone, Debug, PartialEq)]
pub enum VarValue {
    Float(f32),
    Int(i32),
}

pub struct Var {
    pub id: Id,
    pub vtype: VarType,
}

pub enum Statement {
    Assignment(Assignment),
    Condition(Condition),
    Print(Print),
}

pub struct Block {
    pub statements: Vec<Statement>,
}

pub struct Assignment {
    pub id: Id,
    pub value: Expr,
}

#[derive(Debug)]
pub enum ExpressionOp {
    Gt,
    Lt,
    LtGt,
}
#[derive(Debug)]
pub struct ExprRhs {
    pub op: ExpressionOp,
    pub rhs: Exp,
}
#[derive(Debug)]
pub struct Expr {
    pub lhs: Exp,
    pub rhs: Option<ExprRhs>,
}

#[derive(Debug)]
pub enum ExpOp {
    Add,
    Sub,
}
#[derive(Debug)]
pub struct ExpRhs {
    pub op: ExpOp,
    pub rhs: Term,
}
#[derive(Debug)]
pub struct Exp {
    pub lhs: Term,
    pub rhs: Option<ExpRhs>,
}

#[derive(Debug)]
pub enum Factor {
    ParenExpr(Box<Expr>),
    ConstantVal(VarValue),
}

#[derive(Debug)]
pub enum TermOp {
    Mul,
    Div,
}
#[derive(Debug)]
pub struct TermBOp {
    pub lhs: Term,
    pub op: TermOp,
    pub rhs: Term,
}
#[derive(Debug)]
pub enum Term {
    Factor(Factor),
    BOp(Box<TermBOp>),
}

pub struct Condition {
    pub expression: Expr,
    pub then_block: Block,
    pub else_block: Option<Block>,
}

pub enum PrintType {
    Expression(Expr),
    Str(String),
}
pub struct Print {
    pub output: Vec<PrintType>,
}
