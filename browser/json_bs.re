[@bs.val] external jsonify : 'a => string = "JSON.stringify";

let result_to_string = (result) => jsonify(result);

let resultType_to_string = (result) => jsonify(result);
