open Alang.Scanner

let main (_ : unit) =
  print_string "> ";
  let line = read_line () in
  if line <> "exit" then scan line 0 else []

let r =
  let l = ref (main ()) in
  while List.length !l > 0 do
    print_tokens !l;
    l := main ()
  done

let () = r
