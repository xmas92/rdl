use rdl_parse::{grammar::*, runtime::Context};
use std::io;
use std::io::Write;

fn test_ast() -> io::Result<()> {
    let mut input = String::new();

    print!("Type something: ");
    io::stdout().flush()?;

    io::stdin().read_line(&mut input)?;

    println!("You typed: {}", input.trim());
    println!();

    let ast = rdl::program(&input.trim());

    match ast {
        Ok(forms) => {
            let code = forms
                .iter()
                .map(|f| f.unparse())
                .collect::<Vec<String>>()
                .join(" ");
            println!("Ast: {:?}", forms);
            println!();
            println!("Code: {}", code);
            println!();
            let other_forms = rdl::program(&code.as_str()).unwrap();
            assert_eq!(forms, other_forms);
            assert_eq!(
                code,
                other_forms
                    .iter()
                    .map(|f| f.unparse())
                    .collect::<Vec<String>>()
                    .join(" ")
            );
        }
        Err(error) => println!("{:?}", error),
    }
    let context = Context::get_global_context();
    compile("(defmacro defn [name & body] '((quote def) name (cons (quote fn) (cons name body))))")
        .unwrap();
    compile("(defmacro -> [x & forms] (loop [x x forms forms] (if forms (let [form (first forms) threaded (if (seq? form) '((first form) x @(next form)) '(form x))] (recur threaded (next forms))) x)))").unwrap();
    compile("(defmacro ->> [x & forms] (loop [x x forms forms] (if forms (let [form (first forms) threaded (if (seq? form) '((first form) @(next form) x) '(form x))] (recur threaded (next forms))) x)))").unwrap();
    let prog = compile(&input.trim());

    match &prog {
        Ok(forms) => println!("Prog: {:?}", forms),
        Err(error) => println!("{:?}", error),
    };
    println!();
    println!();
    if let Ok(prog) = prog {
        let values: Vec<_> = prog
            .into_iter()
            .map(|v| v.evaluate_global_context())
            .collect();
        println!("{:?}", values);
    };
    println!();
    Context::debug_print_global_complement(&context);
    println!();

    Ok(())
}

fn main() -> io::Result<()> {
    test_ast()
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_01() {
        assert_eq!(2 + 2, 4);
    }
}
