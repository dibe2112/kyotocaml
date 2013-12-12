open Kyotocaml.KCString;;

let lks = [of_string "A"; of_string "B"; of_string "C"; of_string "D"];;
print_endline(to_string(concats lks "  "));;
print_endline(to_string(concat lks (of_string "  ")));;
