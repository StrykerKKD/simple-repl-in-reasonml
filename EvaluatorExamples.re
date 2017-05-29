open Core.Std;

let words value => value |> String.strip |> String.split on::' ';
let unwords = String.concat sep::" ";

let capitalizer expr => expr |> words |> List.map f::String.capitalize |> unwords;

let simple_calc expr => {
    let read = Int.of_string;
    let show = Int.to_string;
    let keywords = words expr;
    
    switch keywords {
        | [x, "+", y] => read x + read y |> show
        | [x, "-", y] => read x - read y |> show
        | [x, "*", y] => read x * read y |> show
        | [x, "/", y] => read x / read y |> show
        | _ => "That's too hard! :("
    }
};

module StringSet = Set.Make String;
let smile_set = StringSet.of_list ["smile", "face"];
let car_set = StringSet.of_list ["car", "transport"];
let cat_set = StringSet.of_list ["cat", "meow", "purr"];
let ball_set = StringSet.of_list ["ball", "sport"];
let food_set = StringSet.of_list ["food"];

let emoji_finder expr => {    
    let keywords = words expr;
    let keyword_set = StringSet.of_list keywords;
    let intersect_exists set1 set2 => StringSet.inter set1 set2 |> StringSet.is_empty |> not;
    
    if (intersect_exists keyword_set smile_set){
        "😀  😃  😄  😁  😆  😅  😂"
    } else if (intersect_exists keyword_set car_set) {
        "🚗  🚕  🚙  🏎  🚓  🚑"
    } else if (intersect_exists keyword_set cat_set) {
        "😺  😸  😻  😼  🙀"
    } else if (intersect_exists keyword_set ball_set) {
        "⚽️  🏀  🏈  ⚾️  🎾  🎱"
    } else if (intersect_exists keyword_set food_set) {
        "🍏  🍎  🍐  🍊  🍋  🍌  🍉  🍇  🍓  🍒  🍑"
    } else {
        "👾  ⁉️"
    }
};