
let argv = Sys.argv;

let readChan chan => {
  let lines = ref [];
  try (
    while true {
      lines := [input_line chan, ...!lines]
    }
  ) {
  | End_of_file => ()
  };
  let lines = List.rev !lines;
  String.concat "\n" lines
};

let readStdin () => readChan stdin;
let readFile x => open_in x |> readChan;
