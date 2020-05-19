![ppltr](docs/logo/logo.svg?sanitize=true)

# A Populator for a peephole optimizer

## Installation and Running

The easiest way to install `ppltr` is using [opam](https://opam.ocaml.org/).
Simply run `opam install .` after cloning the repository.
Afterwards one can run `ppltr -help` for help.

## Evaluation

1. generate `ebso` blocks from [`01-contracts.csv`](eval/01-contracts.csv)
   * call: `ppltr -mode BG eval/01-contracts.csv eval/02-ebso-blocks.csv`
   * output: [`02-ebso-blocks.csv`](eval/02-ebso-blocks.csv)

2. from [`02-ebso-blocks.csv`](eval/02-ebso-blocks.csv) generate
   windowed `ebso` blocks for optimization
   * call: `ppltr -mode WG eval/02-ebso-blocks.csv eval/03-windowed-blocks.csv`
   * output: [`03-windowed-blocks.csv`](eval/03-windowed-blocks.csv)

3. find optimizations
   1. call to ebso
	  * call: `ebso -timeout 900 -translation-validation 256 -direct <ebso-block>`
   2. select from [`04-ebso-results.csv`](eval/04-ebso-results.csv) optimizations with strict gas savings
	  * call: `ppltr -mode OG eval/04-ebso-results.csv eval/05-optimizations.csv`
	  * output: [`05-optimizations.csv`](eval/05-optimizations.csv)

4. generate rules
   1. call to sorg
	  * call: `sorg -timeout 900 <lhs> <rhs>` 
   2. combine rules from [`06-sorg-results.csv`](eval/06-sorg-results.csv)
	  * call: `ppltr -mode RG eval/06-sorg-results.csv eval/07-rules.csv`
	  * output: [`07-rules.csv`](eval/07-rules.csv)

5. rewrite [`07-rules.csv`](eval/07-rules.csv) with `-rules` [`07-rules.csv`](eval/07-rules.csv)
   * call: `ppltr -mode RR eval/07-rules.csv eval/08-reduced-rules.csv -rules eval/07-rules.csv`
   * output: [`08-reduced-rules.csv`](eval/08-reduced-rules.csv)

6. apply  [`08-reduced-rules.csv`](eval/08-reduced-rules.csv) on  [`02-ebso-blocks.csv`](eval/02-ebso-blocks.csv)
   * call: `ppltr -mode BR -rules eval/08-reduced-rules.csv eval/02-ebso-blocks.csv eval/09-reduced-ebso-blocks.csv`
   * output: [`09-reduced-ebso-blocks.csv`](eval/09-reduced-ebso-blocks.csv)
   
7. repeat 1. to 5. with [`09-reduced-ebso-blocks.csv`](eval/09-reduced-ebso-blocks.csv)

8. take [`15-combined-sorg-results.csv`](eval/15-combined-sorg-results.csv)
   * call: `ppltr -mode RG eval/15-combined-sorg-results.csv eval/16-rules.csv`
   * call: `ppltr -mode RR eval/16-rules.csv eval/17-reduced-rules.csv -rules eval/16-rules.csv`
   
9. final rules: [`17-reduced-rules.csv`](eval/17-reduced-rules.csv)

10. to apply [`17-reduced-rules.csv`](eval/17-reduced-rules.csv) on [`01-contracts.csv`](eval/01-contracts.csv)
	* call: `ppltr -mode CR eval/01-contracts.csv eval/19-reduced.csv -rules eval/17-reduced-rules.csv`
