open EvaluatorExamples;

let read () => {
    print_string "REPL>";
    read_line ()
};

/**
 * Try the following ones from `EvaluatorExamples.re`:
 * capitalizer input
 * simple_calc input
 * emoji_finder input
 */
let eval input => input;

let print = print_endline;

let rec loop () => {
    let input = read ();
    switch input {
        | ":quit" | ":q" => ()
        | _ => {
            print (eval input);
            loop ()
        }
    };
};

loop ();