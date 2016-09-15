
type fromMain = (string, string);
type fromWorker =
  | GrammarGood float float /* time to parse, time to convert */
  | GrammarBad PackTypes.Result.partial /* how much was parsed? */
  | InputGood PackTypes.result float /* time to parse */
  | InputBad PackTypes.Result.partial /* how much was parsed? */
