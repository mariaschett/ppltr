# PPLTR

## Evaluation

1. generate ebso blocks for optimization
   * __input__: `01-contracts.csv` with header
   ```
   address,bytecode
   ```
   * __call__: `ppltr -mode BG 01-contracts.csv 02-ebso-blocks.csv`
   * __output__: `02-ebso-blocks.csv` with header
   ```
   source bytecode,source opcode,source instruction count,stack depth, uninterpreted count,storage access count,instances
   ```

2. find optimizations
   1. call to ebso
	  * __call__: `ebso -timeout 900 -translation-validation 256 -direct <ebso-block>`
   2. select optimizations with strict gas savings
	  * __input__: `03-ebso-results.csv` with header
	  ```
	  source bytecode,target bytecode,target opcode,target instruction count,source gas,target gas,gas saved,known optimal,translation validation
	  ```
	  * __call__: `ppltr -mode OG examples/03-ebso-results.csv examples/04-optimizations.csv`
	  * __output__: `04-optimizations.csv` with header
	  ```
	  lhs,rhs
	  ```
	
3. generate rules
   1. call to sorg
	  * __call__: `sorg/sorg.sh -timeout 900`
   2. combine rules
	  * __input__: `05-sorg-results.csv` with header
	  ```
	  rule lhs,rule rhs,vars,gas saved,optimization source,optimization target,tpdb
	  ```
	  * __call__: `ppltr 05-sorg-results.csv 06-rules.csv -mode RG > stats.txt`
	  * __output__: `06-rules.csv`
