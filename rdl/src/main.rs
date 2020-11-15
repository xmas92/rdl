use rdl_lib::{grammar::*, runtime::Context};
use std::io;
use std::io::Write;

fn test_ast() -> io::Result<()> {
    let context = Context::get_global_context();

    compile("(defmacro defn [name & body] '((quote def) name (cons (quote fn) (cons name body))))")
        .unwrap();
    compile("(defmacro -> [x & forms] (loop [x x forms forms] (if forms (let [form (first forms) threaded (if (seq? form) '((first form) x @(rest form)) '(form x))] (recur threaded (next forms))) x)))").unwrap();
    compile("(defmacro ->> [x & forms] (loop [x x forms forms] (if forms (let [form (first forms) threaded (if (seq? form) '((first form) @(rest form) x) '(form x))] (recur threaded (next forms))) x)))").unwrap();
    compile("(defmacro as-> [expr name & forms] '((quote let) [name expr @(sequence (interleave (repeat name) (butlast forms)))] (if forms (last forms) name)))").unwrap();
    loop {
        // (def a (-> 5 repeat (->> (take 5)) (as-> n (interleave n n)))
        let mut input = String::new();
        print!("Type something: ");
        io::stdout().flush()?;

        io::stdin().read_line(&mut input)?;

        println!("You typed: {}", input.trim());
        println!();

        if input.trim() == "exit" {
            break;
        }
        if input.trim() == "context" {
            println!();
            Context::debug_print_global_complement(&context);
            println!();
            continue;
        }

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
            Err(error) => {
                println!("{:?}", error);
                println!(
                    "{}£{}",
                    input
                        .chars()
                        .take(error.location.offset)
                        .collect::<String>(),
                    input
                        .chars()
                        .skip(error.location.offset)
                        .collect::<String>()
                );
                continue;
            }
        }
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
    }

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
