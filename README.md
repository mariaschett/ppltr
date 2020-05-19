![ppltr](docs/logo/logo.svg?sanitize=true)

# A PoPuLaToR for a PeePhoLe opTimizeR

## Evaluation

1. generate `ebso` blocks from [`01-contracts.csv`](eval/01-contracts.csv)
   * __call__: `ppltr -mode BG eval/01-contracts.csv eval/02-ebso-blocks.csv`
   * __output__: [`02-ebso-blocks.csv`](eval/02-ebso-blocks.csv)

2. from [`02-ebso-blocks.csv`](eval/02-ebso-blocks.csv) generate
   windowed `ebso` blocks for optimization
   * __call__: `ppltr -mode WG eval/02-ebso-blocks.csv eval/03-windowed-blocks.csv`
   * __output__: [`03-windowed-blocks.csv`](eval/03-windowed-blocks.csv)

3. find optimizations
   1. call to ebso
	  * __call__: `ebso -timeout 900 -translation-validation 256 -direct <ebso-block>`
   2. select from [`04-ebso-results.csv`](eval/04-ebso-results.csv) optimizations with strict gas savings
	  * __call__: `ppltr -mode OG eval/04-ebso-results.csv eval/05-optimizations.csv`
	  * __output__: [`05-optimizations.csv`](eval/05-optimizations.csv)

3. generate rules
   1. call to sorg
	  * __call__: `sorg -timeout 900 <lhs> <rhs>` 
   2. combine rules from [`06-sorg-results.csv`](eval/06-sorg-results.csv)
	  * __call__: `ppltr -mode RG eval/06-sorg-results.csv eval/07-rules.csv`
	  * __output__: [`07-rules.csv`](eval/07-rules.csv)

4. rewrite [`07-rules.csv`](eval/07-rules.csv) with `-rules` [`07-rules.csv`](eval/07-rules.csv)
   * __call__: `ppltr -mode RR eval/07-rules.csv eval/08-reduced-rules.csv -rules eval/07-rules.csv`
   * __output__: [`08-reduced-rules.csv`](eval/08-reduced-rules.csv)

5. apply  [`08-reduced-rules.csv`](eval/08-reduced-rules.csv) on  [`02-ebso-blocks.csv`](eval/02-ebso-blocks.csv)
   * __call__: `ppltr -mode `
   * __output__: [`09-reduced-ebso-blocks.csv`](eval/09-reduced-ebso-blocks.csv)
   
6. repeat 1. to 5. with [`09-reduced-ebso-blocks.csv`](eval/09-reduced-ebso-blocks.csv)

7. take [`15-combined-sorg-results.csv`](eval/15-combined-sorg-results.csv)
   * __call__: `ppltr -mode RG eval/15-combined-sorg-results.csv eval/16-rules.csv`
   * __call__: `ppltr -mode RR eval/16-rules.csv eval/17-reduced-rules.csv -rules eval/16-rules.csv`
   
8. final rules: [`17-reduced-rules.csv`](eval/17-reduced-rules.csv)

9. to apply [`17-reduced-rules.csv`](eval/17-reduced-rules.csv) on [`01-contracts.csv`](eval/01-contracts.csv)
	* __call__: `ppltr -mode CR eval/01-contracts.csv eval/19-reduced.csv -rules eval/17-reduced-rules.csv`
