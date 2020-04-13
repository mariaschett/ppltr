open Core
open Ppltr

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:"ppltr: A PoPuLaToR for a PeePhoLeopTimizeR of EVM bytecode"
    [%map_open
      let bytecode_csv = anon ("BYTECODE" %: string)
      and blocks_fn = anon ("BLOCKS" %: string)
      in
      fun () ->
        let peephole_sz = 5 in
        Csv.Rows.load ~has_header:true bytecode_csv
        |> List.map ~f:(fun r -> Csv.Row.find r "bytecode")
        |> Blk_generator.generate_blks peephole_sz
        |> List.map ~f:Ebso.Printer.show_ebso_snippet
        |> List.cons Ebso.Printer.ebso_snippet_header
        |> Csv.save blocks_fn
    ]
  |> Command.run ~version:"1.0"
