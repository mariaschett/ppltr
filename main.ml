open Core
open Ppltr

type mode =
  | BLOCK_GEN
  | OPTZ_GEN
[@@deriving show { with_path = false }]

let mode_of_string = function
  | "BG" -> BLOCK_GEN
  | "OG" -> OPTZ_GEN
  | _ -> failwith "Unknown mode"

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:"ppltr: A PoPuLaToR for a PeePhoLeopTimizeR of EVM bytecode"
    [%map_open
      let in_csv = anon ("INPUT" %: string)
      and out_csv = anon ("OUTPUT" %: string)
      and peephole_sz = flag "peephole-size" (optional_with_default 6 int)
          ~doc:"sz maximal size of the peephole window (default: 6)"
      and mode = flag "mode"
          (required (Arg_type.create mode_of_string))
          ~doc:"mode BG or OG"      in
      fun () ->
        match mode with
        | BLOCK_GEN ->
          Csv.Rows.load ~has_header:true in_csv
          |> List.map ~f:(fun r -> Csv.Row.find r "bytecode")
          |> Blk_generator.generate_blks peephole_sz
          |> List.rev_map ~f:(fun (p, c) -> Ebso.Printer.show_ebso_snippet p @ [[%show: int] c])
          |> List.cons (Ebso.Printer.ebso_snippet_header @ ["instances"])
          |> Csv.save out_csv
        | OPTZ_GEN -> ()
    ]
  |> Command.run ~version:"1.0"
