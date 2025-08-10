let () =
  let open Alcotest in
  run "Typing"
    [
      ("Expression", Test_expr.tests);
      ("Statement", Test_stmt.tests);
      ("Program", Test_program.tests);
    ]
