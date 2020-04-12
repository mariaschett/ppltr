open Core
open Ppltr

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:"ppltr: A PoPuLaToR for a PeePhoLeopTimizeR of EVM bytecode"
    [%map_open
      let byte_code_fns = anon ("FILES" %: string)
      and blocks_fn = anon ("BLOCKS" %: string)
      in
      fun () ->
        let peephole_sz = 5 in
        let bs = Blk_generator.generate_blks (In_channel.read_lines byte_code_fns) peephole_sz in
        Ebso.Printer.ebso_snippet_header :: List.map bs ~f:(Ebso.Printer.show_ebso_snippet)
        |> Csv.save blocks_fn
    ]
  |> Command.run ~version:"1.0"
