Testing math and mermaid in Github rendering:

Commit transactions spend one or more utxo given as list $\underline\phi_{committed}$:

```mermaid
%%{ init: { 'flowchart': { 'curve': 'bumpX', "htmlLabels": false} } }%%
flowchart LR
  initial([Initial]) -- "`*commit*` $\phi_committed$" --> tx
  tx --> output1([Commit])
  committed([Committed]) --> tx
```
