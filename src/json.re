
let result_to_string result => PackTypes.Result.result_to_yojson result |> Yojson.Safe.to_string;
let resultType_to_string result => PackTypes.Result.resultType_to_yojson result |> Yojson.Safe.to_string;
