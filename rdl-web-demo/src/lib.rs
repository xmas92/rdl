use app::AppIn;
use log::Level;
use mogwai::prelude::*;
use rdl_lib::grammar::compile;
use std::panic;
use wasm_bindgen::prelude::*;

#[allow(unused_braces)]
pub(crate) mod app;
#[allow(unused_braces)]
pub(crate) mod context_area;
#[allow(unused_braces)]
pub(crate) mod output_area;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen(start)]
pub fn main() -> Result<(), JsValue> {
    panic::set_hook(Box::new(console_error_panic_hook::hook));
    console_log::init_with_level(Level::Trace).unwrap();
    compile("(defmacro defn [name & body] '((quote def) name (cons (quote fn) (cons name body))))")
        .expect("Failed to compile defn macro");
    compile("(defmacro -> [x & forms] (loop [x x forms forms] (if forms (let [form (first forms) threaded (if (seq? form) '((first form) x @(rest form)) '(form x))] (recur threaded (next forms))) x)))")
    .expect("Failed to compile -> macro");
    compile("(defmacro ->> [x & forms] (loop [x x forms forms] (if forms (let [form (first forms) threaded (if (seq? form) '((first form) @(rest form) x) '(form x))] (recur threaded (next forms))) x)))")
    .expect("Failed to compile ->> macro");
    compile("(defmacro as-> [expr name & forms] '((quote let) [name expr @(sequence (interleave (repeat name) (butlast forms)))] (if forms (last forms) name)))")
    .expect("Failed to compile as-> macro");

    let gizmo = Gizmo::from(app::App::default());
    gizmo.trns.send(&AppIn::Setup);
    let view = View::from(gizmo.view_builder());
    view.run()
}
