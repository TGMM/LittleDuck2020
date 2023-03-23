#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Id(String),
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

struct Program {
    id: String,
    vars: Vec<Var>,
    block: Block,
}

enum VarType {
    Float,
    Int,
}

#[derive(Clone, Debug, PartialEq)]
pub enum VarValue {
    Float(f32),
    Int(i32),
}

struct Var {
    id: String,
    vtype: VarType,
}

enum Statement {
    Assignment(Assignment),
    Condition(Condition),
    Print(Print),
}

struct Block {
    statements: Vec<Statement>,
}

struct Assignment {
    id: String,
    value: VarValue,
}

enum ExpressionOp {
    Gt,
    Lt,
    LtGt,
}
struct Expr {
    lhs: Exp,
    op: ExpressionOp,
    rhs: Exp,
}

enum ExpOp {
    Add,
    Sub,
}
struct Exp {
    lhs: Term,
    op: ExpOp,
    rhs: Term,
}

enum Factor {
    ParenExpr(Box<Expr>),
    ConstantVal(VarValue),
}

enum TermOp {
    Mul,
    Div,
}
struct Term {
    lhs: Factor,
    op: TermOp,
    rhs: Factor,
}

struct Condition {
    expression: Expr,
    then_block: Block,
    else_block: Option<Block>,
}

enum PrintType {
    Expression(Expr),
    Str(String),
}
struct Print {
    output: Vec<PrintType>,
}
