open Core
open Ppltr

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:"ppltr: A PoPuLaToR for a PeePhoLeopTimizeR of EVM bytecode"
    [%map_open
      let bytecode_csv = anon ("BYTECODE" %: string)
      and blocks_fn = anon ("BLOCKS" %: string)
      and peephole_sz = flag "peephole-size" (optional_with_default 6 int)
          ~doc:"sz maximal size of the peephole window (default: 6)"
      in
      fun () ->
        Csv.Rows.load ~has_header:true bytecode_csv
        |> List.map ~f:(fun r -> Csv.Row.find r "bytecode")
        |> Blk_generator.generate_blks peephole_sz
        |> List.rev_map ~f:(fun (p, c) -> Ebso.Printer.show_ebso_snippet p @ [[%show: int] c])
        |> List.cons (Ebso.Printer.ebso_snippet_header @ ["instances"])
        |> Csv.save blocks_fn
    ]
  |> Command.run ~version:"1.0"
