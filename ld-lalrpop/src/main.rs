#[macro_use]
extern crate lalrpop_util;
lalrpop_mod!(pub littleduck);

fn main() {}

#[cfg(test)]
mod test {
    use crate::littleduck::ProgramaParser;

    #[test]
    fn programa_test() {
        let p = ProgramaParser::new();
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

        p.parse(input).unwrap();
    }
}
