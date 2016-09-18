
type fromMain =
  | Change (string, string)
  | Refmt
  ;
type fromWorker =
  | GrammarGood float float /* time to parse, time to convert */
  | GrammarBad PackTypes.Error.partial /* how much was parsed? */
  | InputGood PackTypes.Result.result float /* time to parse */
  | InputPretty string float
  | InputBad PackTypes.Error.partial /* how much was parsed? */
