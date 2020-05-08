# PPLTR

## Evaluation

1. generate ebso blocks
   * __input__: `01-contracts.csv` with header
   ```
   address,block_timestamp,block_number,tx_count,bytecode
   ```
   * __call__: `ppltr -mode BG 01-contracts.csv 02-ebso-blocks.csv`
   * __output__: `02-ebso-blocks.csv` with header
   ```
   source bytecode,source opcode,source instruction count,stack depth, uninterpreted count,storage access count
   ```

2. generate windowed ebso blocks for optimization
   * __input__: `02-ebso-blocks.csv`
   * __call__: `ppltr -mode WG 02-ebso-blocks.csv 03-windowed-blocks.csv`
   * __output__: `03-windowed-blocks.csv` with header
   ```
   source bytecode,source opcode,source instruction count,stack depth, uninterpreted count,storage access count,instances
   ```

3. find optimizations
   1. call to ebso
	  * __call__: `ebso -timeout 900 -translation-validation 256 -direct <ebso-block>`
   2. select optimizations with strict gas savings
	  * __input__: `04-ebso-results.csv` with header
	  ```
	  source bytecode,target bytecode,target opcode,target instruction count,source gas,target gas,gas saved,known optimal,translation validation,solver time
	  ```
	  * __call__: `ppltr -mode OG examples/04-ebso-results.csv examples/05-optimizations.csv`
	  * __output__: `05-optimizations.csv` with header
	  ```
	  lhs,rhs
	  ```

3. generate rules
   1. call to sorg
	  * __call__: `sorg/sorg.sh -timeout 900`
   2. combine rules
	  * __input__: `06-sorg-results.csv` with header
	  ```
	  rule lhs,rule rhs,vars,gas saved,optimization source,optimization target,tpdb
	  ```
	  * __call__: `ppltr -mode RG 06-sorg-results.csv 07-rules.csv > stats.txt`
	  * __output__: `07-rules.csv`
