[@bs.val] external raw_argv : array(string) = "process.argv";

[@bs.module "fs"] [@bs.val] external readFileSync : (string, string) => string = "readFileSync";

let argv = Array.of_list(List.tl(Array.to_list(raw_argv)));

let readStdin = () => failwith("not ompl");

let readFile = (name) => readFileSync(name, "utf8");
