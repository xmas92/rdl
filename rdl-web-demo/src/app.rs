use mogwai::prelude::*;
use rdl_lib::{grammar::*, runtime::Context};
use wasm_bindgen::JsCast;
use web_sys::{HtmlTextAreaElement, InputEvent};

use crate::{
    context_area::{ContextArea, ContextAreaIn},
    output_area::{OutputArea, OutputAreaIn},
};

pub(crate) struct App {
    context_area: Gizmo<ContextArea>,
    output_area: Gizmo<OutputArea>,
    input_string: String,
}

#[derive(Clone)]
pub(crate) enum AppIn {
    Setup,
    ClearTextArea,
    ExecuteInput,
    UpdateText(InputEvent),
    ShowExample(String, String),
}

#[derive(Clone)]
pub(crate) enum AppOut {
    InputText(String),
}

impl Default for App {
    fn default() -> Self {
        App {
            context_area: Gizmo::from(ContextArea::default()),
            output_area: Gizmo::from(OutputArea::default()),
            input_string: String::new(),
        }
    }
}

impl App {
    fn get_examples(&self, tx: &Transmitter<AppIn>) -> ViewBuilder<web_sys::HtmlElement> {
        macro_rules! build_examples {
            ($tx:ident, $(($name:expr,$code:expr,$msg:expr)),*) => {
                builder! {
                    <div>
                        $(
                        <button style:cursor="pointer"
                            on:click=$tx.contra_map(|_| AppIn::ShowExample($code,$msg))>
                            {$name}
                        </button>
                        )*
                    </div>
                }
            };
        }
        build_examples! {
            tx,
            (
                "Values".to_owned(),
r#"(def none nil)
(def ratio 1/2)
(def int 3)
(def bigint 1305088109288580288861997749666299477M)
(def float 1.2304E-5)
(def bigdecimal 130504342424234666881092.8858600288834666199774343446643534549666299477M)
(def string "A string👍")
(def ascii \c)
(def unicode \u1F523)
(def bool true)
(def keyword :AKey)
(def vector [1 2 nil int :a :b \u2714])
(def list_ '(\space \tab \newline 4 5 6))
(def set #{:a :a :b :c 2 3})
(def map {:a 3 :b 5 \d "me"})"#.to_owned(),
r#"There are many different values supported. nil is very much like None in Rust.
You can change the Global Context browser to show user defined values only.
Note: The numerical types are only loosely implemented (and ratio not at all). An unparsable number will panic (irrecoverably) during compilation.
Also the different number types cannot be mixed. Check out the https://github.com/xmas92/rdl/tree/rework-enum branch where numbers have auto promoting
arithmetic implemented.
"#.to_owned()
            ),(
                "Forms".to_owned(),
r#"(def symbol :Value)
(def double (fn self [a] (+ a a)))
(double 4)
(def fac (fn self [n] (if (= n 0) 1 (* n (self (- n 1))))))
(fac 10)
(def fac (fn [n] (loop [n n, acc 1] (if (= n 0) acc (recur (- n 1) (* acc n))))))
(fac 10)
(def mult
  (fn this
    ([] 1)
    ([x] x)
    ([x y] (* x y))
    ([x y & [first & rest]]
      (if rest 
        (recur (this x y) first rest)
        (recur (this x y) first)))))
(mult 1 2 3 4 5 6 7)"#.to_owned(),
r#"Form evaluations is how anything gets done in rdl. Every form is a list, every item in the list gets evaluated first to last. (Exception for macro calls and the if form).
Then the first item is called as a function with the rest of the values as arguments. 
There are a few special forms: 
    'def' is the first, it binds a value to a symbol in the global context. 
    'fn' creates a function with multiple arity. They can also have one variadic arity. Ambiguous definitions will compile but there will be a warring in the console and which function that gets called is undefined.
    'if' will check the first following form if it is truthy and falsy and only then evaluate the following forms, the second if truthy and the third if falsy, if not third is supplied it automatically turns to nil.
    'recur' rewinds the stack and rebinds the variables at the closest recur point. Both 'fn' and 'loop' defines a recur point. recur can however rewind the stack and jump across function arity.
    'loop' creates a recur point by binding variables that can be recurred.
    'let' Variable bindings see destructuring example.
    'try' and 'error' do sort of exist but are not finalized as error mechanisms and error recovery is not yet implemented.
"#.to_owned()
            ),(
                "Falsy".to_owned(),
r#"(defn truthy? [v] (if v true false))
(truthy? nil)
(truthy? false)
(truthy? true)
(truthy? [])
(truthy? [1])
(truthy? #{:a})
(truthy? :a)
(truthy? 1)
(truthy? '())
(truthy? {nil nil})
(truthy? (fn [] nil))"#.to_owned(),
r#"Just like in Clojure nil and false are the only falsy values, everything else is truthy.
"#.to_owned()
            ),(
                "Destructuring".to_owned(),
r#"(= (let [[a b c & d :as e] [1 2 3 4 5 6 7]]
        [a b c d e])
    [1 2 3 [4 5 6 7] [1 2 3 4 5 6 7]])
    
(= (let [[[x1 y1][x2 y2]] [[1 2] [3 4]]]
        [x1 y1 x2 y2])
    [1 2 3 4])

(= (let [[a b & c :as str] "asdjhhfdas"]
        [a b c str])
    [\a, \s, [\d, \j, \h, \h, \f, \d, \a, \s], "asdjhhfdas"])

(= (let [{a :a, b :b, c :c, :as m :or {a 2 b 3}}  {:a 5 :c 6}]
        [a b c m])
    [5 3 6 {:c 6, :a 5}])

(= (let [m {:j 15 :k 16 :ivec [22 23 24 25]}
        {j :j, k :k, i :i, [r s & t :as v] :ivec, :or {i 12 j 13}} m]
            [i j k r s t v])
    [12, 15, 16, 22, 23, [24, 25], [22, 23, 24, 25]])"#.to_owned(),
r#"'let' for allows symbols to be bound to specific values withing the enclosing context.
    Anytime there is symbol binding (loop, fn args, let) destructuring bindings can be used. 
They are pretty much the same as in Clojure. A little more restrictive as the :or binding must be 
a symbol map, (clojure allows arbitrary data but only symbols has any valid behavior). Also & rest vec
will return a structurally sharing vector, not a sequence. 
    And as namespaces or some kind of scoping are not yet implemented there is no notion of ::keywords.
Destructuring examples are taken from Clojure.org
"#.to_owned()
            ),(
                "Macro defn".to_owned(),
r#"(defmacro defn 
    [name & body] 
    '((quote def) name 
        (cons (quote fn) (cons name body))))

(defn a [a] a)
(a [1 2 3])

(defn f ([a] (f a a)) ([a b] (f a b a b))([a b & [c d]] (- (+ a c) (+ b d))))
(f 1)
(f 3 1)
(f 1 3)

(defmacro app [f & args] '(f @(concat @(into (list) args))))
(app + [1 2 3])
(app - [1 2 3] [4 5 6])
(app * [1 2 3] [4 5 6] #{5 6 7})"#.to_owned(),
r#"Macros are a bit different from how they work in Clojure. The rules are pretty simple.
Macros are called during compilation, not during evaluation. 
Arguments to macros are not evaluated before being passed to the macro, however they are macro-compiled, 
which means they are turned transformed from a a AST to a representation where values are realized but evaluations are not.
The resulting value is then decompiled back into code and recompiled. In the console you can see the instantiations; 
something along the line "Macro instantiation decompile into: ...".
Also the quote rules different compared to clojure, they are not recursive (neither unquote or quote) so it has to be explicit. 
It does make it a little bit harder. Like this last app, that tries to mimic apply, it takes any number of collections and turns their values into arguments in a function application. 
So first we must turn our vector of vectors into a list '(into (list) args)' then unquote it with @, to then concatenate the lists, then unquote them again to finally return the quoted final form.
"#.to_owned()
            ),
            (
                "Iterators".to_owned(),
r#"(defn range 
    ([from] (iter 
        (fn 
        ([] from)
        ([state] [state (+ state 1)])
        ([state n] [(+ state n) (+ state n 1) (+ n 1)]))))
    ([from n] (take n (range from))))

(seq (range 0 10))
(seq (zip (interleave (repeat 42) (range 0)) (range 10 10))) 
(seq (iter {:a 0 :b 1 :c 2 :d "Cool"}))
(seq (iter "Also works on strings"))"#.to_owned(),
r#"Iterators are lazy constructions much like in Rust. Create a new iterator by using (iter value) or (iter initial_state generator_function). 
    Iterator automatically create lazy sequences of collection types. If you pass iter a function (iter function) then it will call (iter (function) function) and it assumes 
    that the function is of the form:
        (function) -> initial_state 
        | (function state) -> [next_value, next_state]  
        | (function state index) -> [0_indexed_next_value, next_state, values_taken]
    with  (function state 0) == [(unquote (function state)) 1] == [next_value, next_state, 1]. If all values have been taken next_value can be anything but next_state must be nil. 
    "#.to_owned()
            ),(
                "->".to_owned(),
r#"(defmacro -> 
    [x & forms] 
    (loop [x x forms forms] 
        (if forms 
            (let [form (first forms) 
                  threaded (if (seq? form) 
                               '((first form) x @(rest form)) 
                               '(form x))] 
                 (recur threaded (next forms))) 
        x)))
(-> 1
    (+ 10)
    (- 5)
    (* 7)
    (= 42))"#.to_owned(),
r#"This threading macro should be known to anyone coming from Clojure, it simply a loop which takes every form and put them as the first argument in the next form. 
This allows us to turn nested first calls into a sequential list of actions
"#.to_owned()
            ),(
                "->>".to_owned(),
r#"(defmacro ->> 
    [x & forms] 
    (loop [x x forms forms] 
        (if forms 
            (let [form (first forms) 
                  threaded (if (seq? form) 
                               '((first form) @(rest form) x) 
                               '(form x))] 
                 (recur threaded (next forms))) 
        x)))
(->> (repeat 5)
     (take 6)
     (interleave (repeat 2))
     (seq)
     (apply +)
     (= 42))"#.to_owned(),
r#"Almost the same as '->' but now we put the previous form as the last argument instead. 
"#.to_owned()
            ),(
                "as->".to_owned(),
r#"(defmacro as-> 
    [expr name & forms] 
    '((quote let) [name expr 
                   @(sequence (interleave (repeat name) (butlast forms)))] 
                    (if forms 
                        (last forms) 
                        name)))
(as-> 5 a
      (repeat a)
      (as-> 2 b
            (repeat 2)
            (interleave a b))
      (take 12 a)
      (seq a)
      (apply + a)
      (= a 42))"#.to_owned(),
r#"This is the macro that is most like sequential programming. Here we actually create a list of variable bindings where we get:
(let [name expr
      name form1
      name form2
      name ...]
        last_form)
Very much like a 
let x = expr;
let x = expr1(x);
let x = expr2(x);
In Rust. 
"#.to_owned()
            )
        }
    }
}

impl Component for App {
    type DomNode = HtmlElement;
    type ModelMsg = AppIn;
    type ViewMsg = AppOut;

    fn update(&mut self, msg: &AppIn, tx: &Transmitter<AppOut>, _sub: &Subscriber<AppIn>) {
        match msg {
            AppIn::ClearTextArea => {
                self.input_string = String::new();
                let text_area = document()
                    .get_element_by_id("input-text-area")
                    .expect("Failed to find text area by ID")
                    .dyn_into::<HtmlTextAreaElement>()
                    .expect(r#"Failed cast element with id "input-text-area" into TextArea"#);
                text_area.set_value("");
                text_area.focus().expect("Failed to focus text area");
                tx.send(&AppOut::InputText(String::new()))
            }
            AppIn::UpdateText(e) => {
                let text_area = e
                    .target()
                    .expect("Failed to get target of text area input event")
                    .dyn_into::<HtmlTextAreaElement>()
                    .expect("Failed cast element target of input event into TextArea");
                self.input_string = text_area.value();
                tx.send(&AppOut::InputText(self.input_string.clone()))
            }
            AppIn::ExecuteInput => {
                let text_area = document()
                    .get_element_by_id("input-text-area")
                    .expect("Failed to find text area by ID")
                    .dyn_into::<HtmlTextAreaElement>()
                    .expect(r#"Failed cast element with id "input-text-area" into TextArea"#);
                let input = text_area.value();
                let ast = rdl::program(&input.trim());

                match ast {
                    Ok(forms) => {
                        let code = forms
                            .iter()
                            .map(|f| f.unparse())
                            .collect::<Vec<String>>()
                            .join(" ");
                        log::info!("Ast: {:?}", forms);
                        log::info!("Code: {}", code);
                    }
                    Err(error) => {
                        log::info!("{:?}", error);
                        log::info!(
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
                    }
                }

                self.output_area.trns.send(&OutputAreaIn::AppendText(
                    format!("<={}\n", input.trim())
                        .split("\n")
                        .map(|s| s.to_owned())
                        .collect(),
                ));
                let prog = compile(&input.trim());

                match &prog {
                    Ok(forms) => log::info!("Program: {:?}", forms),
                    Err(error) => {
                        self.output_area
                            .trns
                            .send(&OutputAreaIn::AppendText(vec![format!(
                                "=>Compile Error: {:?}\n",
                                error
                            )]))
                    }
                };
                if let Ok(prog) = prog {
                    let values: Vec<_> = prog
                        .into_iter()
                        .map(|v| format!("{:?}\n", v.evaluate_global_context()))
                        .collect();
                    self.output_area.trns.send(&OutputAreaIn::AppendText(
                        format!("=>{}", values.concat())
                            .split("\n")
                            .map(|s| s.to_owned())
                            .collect(),
                    ));
                    self.input_string = String::new();
                    text_area.set_value(&self.input_string);
                };
                self.context_area
                    .trns
                    .send(&ContextAreaIn::NewContext(Context::get_global_context()));
            }
            AppIn::Setup => {
                log::info!("Sending Welcome Message");
                self.context_area.trns.send(&ContextAreaIn::ResetContext);
                self.output_area.trns.send(&OutputAreaIn::AppendText(vec![
                    "Welcome to RDL web demo. This is a REPL. Submit code above, \
                          look at the examples to get an idea of what is possible and \
                          look in the full global context to see what is available. \
                          See the console for some extra information.\
                          Also check out the repo: https://github.com/xmas92/rdl"
                        .to_owned(),
                ]));
            }
            AppIn::ShowExample(code, msg) => {
                self.input_string = code.clone();
                let text_area = document()
                    .get_element_by_id("input-text-area")
                    .expect("Failed to find text area by ID")
                    .dyn_into::<HtmlTextAreaElement>()
                    .expect(r#"Failed cast element with id "input-text-area" into TextArea"#);
                text_area.set_value(&self.input_string);
                text_area.focus().expect("Failed to focus text area");
                tx.send(&AppOut::InputText(self.input_string.clone()));
                self.output_area.trns.send(&OutputAreaIn::AppendText(
                    msg.split("\n").map(|s| s.to_owned()).collect(),
                ));
            }
        }
    }

    fn view(&self, tx: &Transmitter<AppIn>, rx: &Receiver<AppOut>) -> ViewBuilder<HtmlElement> {
        builder! {
            <div class="flex-container">
                <div class="flex-item-left">
                    { self.context_area.view_builder() }
                </div>
                <div class="flex-item-right">
                    <fieldset>
                        <legend>"Input"</legend>
                        <textarea id="input-text-area" rows="20" autocorrect="off" autocapitalize="off" spellcheck="false"
                            style="width: 100%;"  on:input=tx.contra_map(|e: &Event| {
                                AppIn::UpdateText(e.clone().dyn_into().expect("Failed to cast oninput Event to InputEvent"))
                            })
                        >{(&self.input_string, rx.branch_map(|AppOut::InputText(s)| s.clone()))}</textarea>
                        <button style:cursor="pointer" on:click=tx.contra_map(|_| AppIn::ExecuteInput)>
                            "Submit"
                        </button>
                        <button style:cursor="pointer" on:click=tx.contra_map(|_| AppIn::ClearTextArea)>
                            "Clear"
                        </button>
                        <fieldset>
                            <legend>"Examples"</legend>
                            {self.get_examples(tx)}
                        </fieldset>
                    </fieldset>
                    { self.output_area.view_builder() }
                </div>
            </div>
        }
    }
}
