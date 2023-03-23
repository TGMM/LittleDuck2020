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

pub enum ExpressionOp {
    Gt,
    Lt,
    LtGt,
}
pub struct ExprRhs {
    pub op: ExpressionOp,
    pub rhs: Exp,
}
pub struct Expr {
    pub lhs: Exp,
    pub rhs: Option<ExprRhs>,
}

pub enum ExpOp {
    Add,
    Sub,
}
pub struct ExpRhs {
    pub op: ExpOp,
    pub rhs: Term,
}
pub struct Exp {
    pub lhs: Term,
    pub rhs: Option<ExpRhs>,
}

pub enum Factor {
    ParenExpr(Box<Expr>),
    ConstantVal(VarValue),
}

pub enum TermOp {
    Mul,
    Div,
}
pub struct TermRhs {
    pub op: TermOp,
    pub rhs: Factor,
}
pub struct Term {
    pub lhs: Factor,
    pub rhs: Option<TermRhs>,
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
