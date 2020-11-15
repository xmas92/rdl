[![MIT License](https://img.shields.io/github/license/xmas92/rdl)](https://github.com/xmas92/rdl/blob/master/LICENSE)
![Build and Test](https://github.com/xmas92/rdl/workflows/Build%20and%20Test/badge.svg)


<br />
<p align="center">
  <h3 align="center">RDL: Rust data language</h3>

  <p align="center">
    A hobby implementation of Clojure like Data language with a runtime written in Rust and running both natively and on the web with Wasm.
    <br />
    <a href="https://xmas92.github.io/rdl/"><strong> Try it out online [Wasm REPL] »</strong></a>
    <br />
    <br />
    <a href="#how-it-was-made">Explore how it was made</a>
  </p>
</p>

## Table of Contents

* [About the Project](#about-the-project)
  * [Motivation](#motivation)
  * [Crates Used](#crates-used)
* [How it was made](#how-it-was-made)
* [What is next](#what-is-next)
* [License](#license)
* [Contact](#contact)
* [Acknowledgments](#acknowledgments)

## About The Project

This project initially came from the idea of writing a clojure runtime in a language other than Java. When I first started writing anything it was call [CDL: C++ Data Language](https://github.com/xmas92/cdl) and I wrote it in C++. Because the language ecosystem does not have something as wonderful as `Cargo` and `Crate.io` I started out writing everything from scratch.
I also took this as an opportunity to use all the new not yet released tools of C++. And eventually things broke. So that became an opportunity to finally learn Rust, after just studying it at an academic level.

### Motivation

The idea of having a Data language for rust is not to far fetched. A direct translation of a language like Clojure, which is the status of this project right now, is not the best fit for a such strongly typed language as Rust. However the use of persistent data structures with structural sharing plays very well with the rust type and lifetime systems. A data language which is focused on abstracting away the data access and creating a data oriented way of creating and generating generic rust trait functionality could be a great application for just this.

The of the great benefits with a Data language that can be compiled but also be run interactively in a runtime is that it enables REPL driven development. REPL driven development is great tool for getting fast feedback when iterating on design and business logic.

### Crates Used

* [im](https://crates.io/crates/im)
  * >Blazing fast immutable collection datatypes for Rust.
  * Definitely a well designed library, pretty much the reason I restarted this project in Rust.

    [Docs »](http://immutable.rs/)
* [peg](https://crates.io/crates/peg)
  * >rust-peg is a simple yet flexible parser generator that makes it easy to write robust parsers
  * After failing to write a compile time generated PEG parser in C++, I can only exclaim how much nicer it is in Rust with macros.

    [Docs »](https://github.com/kevinmehall/rust-peg/blob/master/README.md#readme)
* [mogwai](https://crates.io/crates/mogwai)
  * >minimalist, obvious, graphical web application interface
  * Definitely the most interesting of all the Rust Wasm frontends. Not using a shadow-dom and instead just sending messages was a nice change of pace.

    [Docs »](https://docs.rs/mogwai/0.3.3)
* [wasm-bindgen](https://github.com/rustwasm/wasm-bindgen)
  * >Facilitating high-level interactions between Wasm modules and JavaScript
  * I don't think I've ever had a more seamless experience with just getting something that if of this scope to just work out of the box. I truly hope Wasm is the future of the internet.



## How it was made

**TODO** Might add some more information on how this project was made.

However if anyone want so build it should be as easy as just cloning the repo and running `cargo build`. Run `cargo run -p rdl` to run the test *REPL*. It does however require nightly as I ended using some unstable features. Some just because they were nice and some that I wasn't sure how to without (`arc_new_cyclic`), at least without some unsafe code.

If you want to compile the Wasm SPA just follow the commands in the [rdl-web-demo README](rdl-web-demo/README.md). Also you can try it out online this projects github pages.  [[Wasm REPL] »](https://xmas92.github.io/rdl/)

## What is next

There are many things that can be done but I am not sure I will continue iteration on this Clojure like clone.

I already have another branch [rework-enum](https://github.com/xmas92/rdl/tree/rework-enum) where I unified the number runtime types into an auto promoting type. However I feel like that is the wrong way forward. It is to rigid. If I were to take another stab at this Rust data language idea it would this:

* Design a trait oriented syntax with focus on transpiling into Rust trait generics.
* Build a dynamic type runtime based on this which can be evaluated in a REPL
  * With the caveat that it might need some recompilation and glue / tooling to enable interoperability with current projects.

* Create dual compilation modes one with REPL and marshaling and one without
  * Can definitely see the benefit in this if it could be used for Wasm web applications while they are running.

## License

Distributed under the MIT License. See `LICENSE` for more information.

## Contact

Axel Boldt-Christmas - xmas1915@gmail.com

Project Link: [https://github.com/xmas92/rdl](https://github.com/xmas92/rdl)

## Acknowledgments

* The great resource that Crates.io and Docs.rs are. Standardizing and centralizing both build tools and documentation makes things so much easier.
