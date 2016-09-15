
external jsonify : 'a => string = "JSON.stringify" [@@bs.val];

let result_to_string result => jsonify result;
let resultType_to_string result => jsonify result;
