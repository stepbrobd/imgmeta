let test_smoke () = Alcotest.(check pass) "scaffold is alive" () ()

let () =
  Alcotest.run "imgmeta" [ "scaffold", [ Alcotest.test_case "smoke" `Quick test_smoke ] ]
;;
