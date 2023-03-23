#![feature(let_chains)]

mod ast;
mod lexer;
mod littleduck;

fn main() {}

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
            my_var = 10;
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
