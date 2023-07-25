"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[8793],{19257:(e,t,a)=>{a.d(t,{Z:()=>i});var n=a(67294),o=a(60614);const r={terminalWindow:"terminalWindow_wGrl",terminalWindowHeader:"terminalWindowHeader_o9Cs",terminalWindowBody:"terminalWindowBody_tzdS",row:"row_Rn7G",buttons:"buttons_IGLB",right:"right_fWp9",dot:"dot_fGZE"};function i(e){let{children:t,minHeight:a}=e;const i="string"==typeof t?n.createElement(o.Z,null,t):t;return n.createElement("div",{className:r.terminalWindow,style:{minHeight:a}},n.createElement("div",{className:r.terminalWindowHeader},n.createElement("div",{className:r.buttons},n.createElement("span",{className:r.dot,style:{background:"#f25f58"}}),n.createElement("span",{className:r.dot,style:{background:"#fbbe3c"}}),n.createElement("span",{className:r.dot,style:{background:"#58cb42"}}))),n.createElement("div",{className:r.terminalWindowBody},i))}},47877:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>s,contentTitle:()=>i,default:()=>p,frontMatter:()=>r,metadata:()=>d,toc:()=>l});var n=a(87462),o=(a(67294),a(3905));a(19257);const r={sidebar_position:5},i="Setup",d={unversionedId:"tutorial/using_hydra/using-hydra-part-2",id:"tutorial/using_hydra/using-hydra-part-2",title:"Setup",description:"To showcase the protocol, we consider a minimal setup of two participants that together want to open a hydra head, call these two Bob and Alice. To start, we enter a nix-shell in the hydra repo and create a directory to hold some setup files.",source:"@site/docs/tutorial/using_hydra/using-hydra-part-2.md",sourceDirName:"tutorial/using_hydra",slug:"/tutorial/using_hydra/using-hydra-part-2",permalink:"/head-protocol/fr/docs/tutorial/using_hydra/using-hydra-part-2",draft:!1,editUrl:"https://github.com/input-output-hk/hydra/tree/master/docs/docs/tutorial/using_hydra/using-hydra-part-2.md",tags:[],version:"current",sidebarPosition:5,frontMatter:{sidebar_position:5},sidebar:"defaultSidebar",previous:{title:"Preliminaries",permalink:"/head-protocol/fr/docs/tutorial/using_hydra/using-hydra-part-1"},next:{title:"Running",permalink:"/head-protocol/fr/docs/tutorial/using_hydra/using-hydra-part-3"}},s={},l=[],c={toc:l},h="wrapper";function p(e){let{components:t,...a}=e;return(0,o.kt)(h,(0,n.Z)({},c,a,{components:t,mdxType:"MDXLayout"}),(0,o.kt)("h1",{id:"setup"},"Setup"),(0,o.kt)("p",null,"To showcase the protocol, we consider a minimal setup of two participants that together want to open a hydra head, call these two Bob and Alice. To start, we enter a nix-shell in the ",(0,o.kt)("inlineCode",{parentName:"p"},"hydra")," repo and create a directory to hold some setup files."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},"mkdir test-head\nmkdir test-head/Bob\nmkdir test-head/Alice\n")),(0,o.kt)("p",null,"Then we create for both participants a Cardano key pair and calculate its associated address. We do this with"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},"cardano-cli address key-gen --verification-key-file test-head/Bob/BobCardano.vk --signing-key-file test-head/Bob/BobCardano.sk\ncardano-cli address build --payment-verification-key-file test-head/Bob/BobCardano.vk --testnet-magic 2 --out-file test-head/Bob/BobCardano.addr\n")),(0,o.kt)("p",null,"and"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},"cardano-cli address key-gen --verification-key-file test-head/Alice/AliceCardano.vk --signing-key-file test-head/Alice/AliceCardano.sk\ncardano-cli address build --payment-verification-key-file test-head/Alice/AliceCardano.vk --testnet-magic 2 --out-file test-head/Alice/AliceCardano.addr\n")),(0,o.kt)("p",null,"Next we fund the wallets of Alice"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},"cat ./test-head/Alice/AliceCardano.addr\n")),(0,o.kt)("p",null,"via the preview testnet ",(0,o.kt)("a",{href:"https://docs.cardano.org/cardano-testnet/tools/faucet"},"faucet"),". You can check the balance of this address via"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},"cardano-cli query utxo --testnet-magic 2 --address $(cat ./test-head/Alice/AliceCardano.addr)\n")),(0,o.kt)("p",null,"We can use the following script to split these funds with the wallet of Bob."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},"#!/usr/bin/env bash\n\nfullInput=$(cardano-cli query utxo --testnet-magic 2 --address $(cat ./test-head/Alice/AliceCardano.addr) | tail -n 1)\ninputTxRef=$(echo $fullInput | awk '{print $1}')\ninputTxId=$(echo $fullInput | awk '{print $2}')\ninputValue=$(echo $fullInput | awk '{print $3}')\n\ncardano-cli transaction build --babbage-era --testnet-magic 2 \\\n    --tx-in $inputTxRef#$inputTxId \\\n    --tx-out $(cat ./test-head/Bob/BobCardano.addr)+$(($inputValue / 2))\\\n    --change-address $(cat ./test-head/Alice/AliceCardano.addr) \\\n    --out-file ./test-head/splitTx.tx\n\ncardano-cli transaction sign --testnet-magic 2 \\\n        --signing-key-file ./test-head/Alice/AliceCardano.sk \\\n        --tx-body-file ./test-head/splitTx.tx \\\n        --out-file ./test-head/splitTx.signed\n\nrm ./test-head/splitTx.tx\n\ncardano-cli transaction submit --testnet-magic 2 \\\n    --tx-file ./test-head/splitTx.signed\n\nrm ./test-head/splitTx.signed\n")),(0,o.kt)("p",null,"We can check the balance of both addresses with"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},"cardano-cli query utxo --address $(cat ./test-head/Alice/AliceCardano.addr) --testnet-magic 2\ncardano-cli query utxo --address $(cat ./test-head/Bob/BobCardano.addr) --testnet-magic 2\n")),(0,o.kt)("admonition",{title:"Fuel is deprecated and will be removed in future Hydra versions.",type:"warning"},(0,o.kt)("p",{parentName:"admonition"},"Please take a look at ",(0,o.kt)("a",{parentName:"p",href:"/docs/getting-started/quickstart#external-commits"},"external-commits"),".")),(0,o.kt)("p",null,"Next we will mark some funds at each address so that the hydra-node can use these to pay for the hydra transactions and make sure that these are not committed in the head. Besides preventing having no funds left to close the head or contest to a false checkpoint, it also acts as the fuel for other stages of the protocol. These commands and script will make an output with a specific datum that the hydra node recognizes as fuel. Before we use the script make sure that ",(0,o.kt)("inlineCode",{parentName:"p"},"jq")," is in your path, if not use"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},"nix-shell -p jq\n")),(0,o.kt)("p",null,"Then, to execute the script use"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},"export CCLI_CMD=$(which cardano-cli)\n./sample-node-config/gcp/scripts/fuel-testnet.sh ./preview-testnet/ ./test-head/Alice/AliceCardano.sk 4900000000\n./sample-node-config/gcp/scripts/fuel-testnet.sh ./preview-testnet/ ./test-head/Bob/BobCardano.sk 4900000000\n")),(0,o.kt)("p",null,"This will mark about 100 ada as fuel for transactions hydra related. The other funds can be committed to the head."),(0,o.kt)("p",null,"Now we are going to set up the Hydra keys for the two parties. We can do this via the ",(0,o.kt)("inlineCode",{parentName:"p"},"hydra-tool")," executable. Before we use this, we build this tool along with the ",(0,o.kt)("inlineCode",{parentName:"p"},"hydra-node")," package with"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},"cabal build hydra-tools\ncabal build hydra-node\n")),(0,o.kt)("p",null,"This can take some time. After this is done, we create two aliases that access these two binaries (you can also add an export to your ",(0,o.kt)("inlineCode",{parentName:"p"},".bashrc")," file)"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},"alias hydra-tools=full/path/to/dist-newstyle/build/x86_64-linux/ghc-8.10.7/hydra-node-0.8.0/x/hydra-tools/build/hydra-tools/hydra-tools\nalias hydra-node=full/path/to/dist-newstyle/build/x86_64-linux/ghc-8.10.7/hydra-node-0.8.0/x/hydra-node/build/hydra-node/hydra-node\n")),(0,o.kt)("p",null,"to make them locally available. Alternatively, you can download the prebuilt binaries via this ",(0,o.kt)("a",{href:"https://github.com/input-output-hk/hydra-poc/releases/tag/0.8.0"},"link"),". Then we can use"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},"hydra-tools gen-hydra-key --output-file ./test-head/Alice/AliceHydra\nhydra-tools gen-hydra-key --output-file ./test-head/Bob/BobHydra\n")),(0,o.kt)("p",null,"We see the creation of the files ",(0,o.kt)("inlineCode",{parentName:"p"},"AliceHydra.sk")," and ",(0,o.kt)("inlineCode",{parentName:"p"},"AliceHydra.vk")," (similar for Bob). These are the cryptographic key pairs that sign each snapshot for Alice and Bob."),(0,o.kt)("p",null,"We still need one thing, before we spin up the two hydra-nodes, that is the protocol parameter that we will use in our test head. We will use the protocol parameters that are the same on the testnet, but with a small tweak that there are no fees! Copy these settings from the ",(0,o.kt)("inlineCode",{parentName:"p"},"hydra")," directory with"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},"cp hydra-cluster/config/protocol-parameters.json ./test-head/protocol-parameters.json\n")),(0,o.kt)("p",null,"Next we assign Alice the localhost address ",(0,o.kt)("inlineCode",{parentName:"p"},"127.0.0.1:5001")," and Bob ",(0,o.kt)("inlineCode",{parentName:"p"},"127.0.0.1:5002"),". As stated in the protocol outline, we need these four things to initiate the communication of a head"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"An IP address + port of their machine that will run the Hydra node."),(0,o.kt)("li",{parentName:"ul"},"A Hydra verification key to identify them in the head."),(0,o.kt)("li",{parentName:"ul"},"A Cardano verification key to identify them on the blockchain."),(0,o.kt)("li",{parentName:"ul"},"The protocol parameters that they want to use in the Hydra head.")),(0,o.kt)("p",null,"Which we have set up above, now we can start a hydra node for each party."),(0,o.kt)("p",null,"Now we open two terminals and enter a nix-shell for each from the ",(0,o.kt)("inlineCode",{parentName:"p"},"hydra")," directory, and reassign the aliases as above for the ",(0,o.kt)("inlineCode",{parentName:"p"},"hydra-node"),". Before we launch the node, we first change directory to `test-head/Alice. We do this such that each node (that of Alice or Bob) does not interfere with the other. The node might produce some temporary files, so it is good practice to keep nodes separated at run time. After that, we launch a hydra-node for Alice with"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},"hydra-node \\\n    --node-id 1 --port 5001 --api-port 4001 \\\n    --peer 127.0.0.1:5002 \\\n    --hydra-signing-key AliceHydra.sk \\\n    --hydra-verification-key ../Bob/BobHydra.vk \\\n    --hydra-scripts-tx-id 4081fab39728fa3c05c0edc4dc7c0e8c45129ca6b2b70bf8600c1203a79d2c6d \\\n    --cardano-signing-key AliceCardano.sk \\\n    --cardano-verification-key ../Bob/BobCardano.vk \\\n    --ledger-protocol-parameters ../protocol-parameters.json \\\n    --network-id 2 \\\n    --node-socket ../../preview-testnet/node.socket\n")),(0,o.kt)("p",null,"Similarly, we change directories to `test-head/Bob and execute for Bob"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},"hydra-node \\\n    --node-id 2 --port 5002 --api-port 4002 \\\n    --peer 127.0.0.1:5001 \\\n    --hydra-signing-key BobHydra.sk \\\n    --hydra-verification-key ../Alice/AliceHydra.vk \\\n    --hydra-scripts-tx-id 4081fab39728fa3c05c0edc4dc7c0e8c45129ca6b2b70bf8600c1203a79d2c6d \\\n    --cardano-signing-key BobCardano.sk \\\n    --cardano-verification-key ../Alice/AliceCardano.vk \\\n    --ledger-protocol-parameters ../protocol-parameters.json \\\n    --network-id 2 \\\n    --node-socket ../../preview-testnet/node.socket\n")),(0,o.kt)("p",null,"Here a few things stand out, first we see that each party adds the other as a ",(0,o.kt)("inlineCode",{parentName:"p"},"--peer"),". Secondly, each party adds its own Cardano and Hydra signing key and peers Cardano and Hydra verification key. We also see that each node opens a local API for that party to communicate with its own node (using the ",(0,o.kt)("inlineCode",{parentName:"p"},"--api-port")," flag). Lastly, we see the flag ",(0,o.kt)("inlineCode",{parentName:"p"},"--hydra-scripts-tx-id")," followed by a hash. This is a transaction hash on the preview network that contains the hydra protocol scripts in its outputs. This way, we can reference these in our transactions to save on fees when making onchain transactions."))}p.isMDXComponent=!0}}]);