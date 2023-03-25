#![feature(let_chains)]

use compiler::compile_ld;

mod ast;
mod compiler;
pub mod lexer;
mod littleduck;
pub mod parser;
mod token;

fn main() {
    let input = r#"
    program my_program;
    var a, x, y, z: int;
    var f: float;
    {
        a = 10 + 10;
        a = 10 > 10;
        a = 10 < 10;
        a = 10 <> 10;
        x = 10 + 5 * 30;
        y = (10 + 5) * 30;
        z = 10 + (5 * 30);
        a = y;

        print("test");
        print("test", a, 10);
    }
    "#;

    compile_ld(input).unwrap();
}

#[cfg(test)]
mod test {
    use crate::littleduck::programa_parser;

    #[test]
    fn programa_test() {
        let input = r#"
        program my_program;
        var my_var: int;
        var my_other_var, my_other_other_var: float;
        {
            my_var = 10 + 10;
            my_var = 10 > 10;
            my_var = 10 < 10;
            my_var = 10 <> 10;
            my_var = 10 + 5 * 30;
            my_var = (10 + 5) * 30;
            my_var = 10 + (5 * 30);
            
            print("test");
            print("test", my_var, 10);
        }
        "#;

        let no_space_input: String = input.split_whitespace().collect();
        programa_parser(&no_space_input).unwrap();
    }
}
