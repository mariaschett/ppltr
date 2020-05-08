open Core
open Ppltr

type mode =
  | BLOCK_GEN
  | WINDOW_GEN
  | OPTZ_GEN
  | RULE_GEN
[@@deriving show { with_path = false }]

let mode_of_string = function
  | "BG" -> BLOCK_GEN
  | "WG" -> WINDOW_GEN
  | "OG" -> OPTZ_GEN
  | "RG" -> RULE_GEN
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
          ~doc:"mode BG or WG or OG or RG"
      in
      fun () ->
        match mode with
        | BLOCK_GEN -> Blk_generator.write_blks in_csv out_csv
        | WINDOW_GEN -> Blk_generator.write_windowed_blks in_csv out_csv peephole_sz
        | OPTZ_GEN -> Optz_generator.write_optz in_csv out_csv
        | RULE_GEN ->
          let (rules, stats) = Rule_generator.compute_results in_csv in
          Rule_generator.write_rules out_csv rules;
          Rule_generator.print_stats stats rules;

    ]
  |> Command.run ~version:"1.0"
