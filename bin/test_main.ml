let test_parens_p () =
  let test_cases = [
    "((()))", "((()))";
    "()", "()";
    "(())", "(())";
    "((())", "((())";
    "()()", "()()";
    "((())())", "((())())";
    "(", "";
    ")", "";
    "((())", "";
    "(()))", "";
    "((())(", "";
  ] in
  List.iter (fun (input, expected) ->
    let result = parse_string (parens_p (return ())) input in
    if result = Ok () then
      Printf.printf "Test passed: %s\n" input
    else
      Printf.printf "Test failed: %s\n" input
  ) test_cases
  
  let test_parse () =
  let test_cases = [
    "1 + 2", "Success: 1 + 2";
    "3 * 4", "Success: 3 * 4";
    "5 - 6", "Success: 5 - 6";
    "7 / 8", "Success: 7 / 8";
    "9", "Success: 9";
    "10 + 11 * 12", "Success: 10 + 11 * 12";
    "13 - 14 / 15", "Success: 13 - 14 / 15";
    "16 * (17 + 18)", "Success: 16 * (17 + 18)";
    "(19 + 20) / 21", "Success: (19 + 20) / 21";
  ] in
  List.iter (fun (input, expected) ->
    let result = try parse input with Failure msg -> msg in
    if result = expected then
      Printf.printf "Test passed: %s\n" input
    else
      Printf.printf "Test failed: %s\n" input
  ) test_cases