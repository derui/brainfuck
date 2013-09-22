
type data = int array
type program = string

type interpreter = {
  mutable instruction : int;
  mutable data_pointer : int;
  mutable data_array : data;
  program : program;
  in_stream: in_channel;
  out_stream : out_channel;
}

let safe_index_from str from c =
  try
    String.index_from str from c
  with Not_found -> -1

let rev_brace = function
  | '[' -> ']'
  | ']' -> '['
  | _ -> failwith "no brace to be matching"

let search_pair interpret direction searchee =
  let rec search_brace program pos count =
    if count = 0 then
      succ pos
    else if pos >= String.length program then
      failwith "not found match brace, because over program list"
    else
      match String.get program pos with
      | '[' -> search_brace program (direction pos) (succ count)
      | ']' -> search_brace program (direction pos) (pred count)
      | _ -> search_brace program (direction pos) count
  in
  match searchee with
  | '[' -> search_brace interpret.program (succ interpret.instruction) 1
  | ']' -> search_brace interpret.program (pred interpret.instruction) (-1)
  | _ -> failwith "can't search any brace"

let parse instruction interpret =
  let succ_inst interpret = {interpret with instruction = succ interpret.instruction} in
  match instruction with
  | '>' -> succ_inst {interpret with data_pointer = succ interpret.data_pointer}
  | '<' -> succ_inst {interpret with data_pointer = pred interpret.data_pointer}
  | '+' ->
    let v = interpret.data_array.(interpret.data_pointer) in
    interpret.data_array.(interpret.data_pointer) <- succ v;
    succ_inst interpret
  | '-' ->
    let v = interpret.data_array.(interpret.data_pointer) in
    interpret.data_array.(interpret.data_pointer) <- pred v;
    succ_inst interpret
  | '.' ->
    let v = interpret.data_array.(interpret.data_pointer) in
    output_char interpret.out_stream (char_of_int v);
    flush interpret.out_stream;
    succ_inst interpret
  | ',' ->
    let v = input_char interpret.in_stream in
    interpret.data_array.(interpret.data_pointer) <- int_of_char v;
    succ_inst interpret
  | '[' ->
    if interpret.data_array.(interpret.data_pointer) <> 0 then
      succ_inst interpret
    else
      let pair_inst = search_pair interpret succ '[' in
      {interpret with instruction = pair_inst}
  | ']' ->
    if interpret.data_array.(interpret.data_pointer) = 0 then
      succ_inst interpret
    else
      let pair_inst = search_pair interpret pred ']' in
      {interpret with instruction = pair_inst}
  | _ -> succ_inst interpret

let rec loop interpret =
  match String.length interpret.program <= interpret.instruction with
  | true -> exit 0
  | false ->
    let instruction = String.get interpret.program interpret.instruction in
    let interpret = parse instruction interpret in
    loop interpret

let _ =
  let brainfuck = {
    instruction = 0;
    data_pointer = 0;
    data_array = Array.make 30000 0;
    program = Sys.argv.(1);
    in_stream = stdin;
    out_stream = stdout;
  } in

  ignore (loop brainfuck)

