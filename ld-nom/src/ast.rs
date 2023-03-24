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

#[derive(Debug)]
pub struct Program {
    pub id: Id,
    pub vars: Vec<Var>,
    pub block: Block,
}

#[derive(Clone, Copy, Debug)]
pub enum VarType {
    Float,
    Int,
}

#[derive(Clone, Debug, PartialEq)]
pub enum VarValue {
    Float(f32),
    Int(i32),
    Var(Id),
}

#[derive(Debug)]
pub struct Var {
    pub id: Id,
    pub vtype: VarType,
}

#[derive(Debug)]
pub enum Statement {
    Assignment(Assignment),
    Condition(Condition),
    Print(Print),
}

#[derive(Debug)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
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
pub struct ExpBOp {
    pub lhs: Exp,
    pub op: ExpOp,
    pub rhs: Exp,
}
#[derive(Debug)]
pub enum Exp {
    Term(Term),
    BOp(Box<ExpBOp>),
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

#[derive(Debug)]
pub struct Condition {
    pub expression: Expr,
    pub then_block: Block,
    pub else_block: Option<Block>,
}

#[derive(Debug)]
pub enum PrintType {
    Expression(Expr),
    Str(String),
}
#[derive(Debug)]
pub struct Print {
    pub output: Vec<PrintType>,
}
