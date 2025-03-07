#!/usr/bin/env bash

# Seed a "devnet" by distributing fuel to hydra nodes and also some Ada to
# commit to the Hydra Head.
set -eo pipefail

SCRIPT_DIR=$(realpath $(dirname $(realpath $0)))
NETWORK_ID=42

CCLI_CMD=
DEVNET_DIR=/devnet
if [[ -n ${1} ]]; then
    echo >&2 "Using provided cardano-cli command: ${1}"
    $(${1} version > /dev/null)
    CCLI_CMD=${1}
    DEVNET_DIR=${SCRIPT_DIR}/devnet
fi

HYDRA_NODE_CMD=
if [[ -n ${2} ]]; then
    echo >&2 "Using provided hydra-node command: ${2}"
    ${2} --version > /dev/null
    HYDRA_NODE_CMD=${2}
fi

# Invoke cardano-cli in running cardano-node container or via provided cardano-cli
function ccli() {
  ccli_ ${@} --testnet-magic ${NETWORK_ID}
}
function ccli_() {
  if [[ -x ${CCLI_CMD} ]]; then
      ${CCLI_CMD} ${@}
  else
      docker-compose exec cardano-node cardano-cli ${@}
  fi
}

# Invoke hydra-node in a container or via provided executable
function hnode() {
  if [[ -n ${HYDRA_NODE_CMD} ]]; then
      ${HYDRA_NODE_CMD} ${@}
  else
      docker run --rm -it \
        --pull always \
        -v ${SCRIPT_DIR}/devnet:/devnet \
        ghcr.io/input-output-hk/hydra-node:0.11.0 -- ${@}
  fi
}

# Retrieve some lovelace from faucet, marked as "fuel" if requested
function seedFaucet() {
    ACTOR=${1}
    AMOUNT=${2}
    echo >&2 "Seeding a UTXO from faucet to ${ACTOR} with ${AMOUNT}Ł"

    # Determine faucet address and just the **first** txin addressed to it
    FAUCET_ADDR=$(ccli address build --payment-verification-key-file ${DEVNET_DIR}/credentials/faucet.vk)
    FAUCET_TXIN=$(ccli query utxo --address ${FAUCET_ADDR} --out-file /dev/stdout | jq -r 'keys[0]')

    ACTOR_ADDR=$(ccli address build --payment-verification-key-file ${DEVNET_DIR}/credentials/${ACTOR}.vk)

    ccli transaction build --babbage-era --cardano-mode \
        --change-address ${FAUCET_ADDR} \
        --tx-in ${FAUCET_TXIN} \
        --tx-out ${ACTOR_ADDR}+${AMOUNT} \
        --out-file ${DEVNET_DIR}/seed-${ACTOR}.draft >&2
    ccli transaction sign \
        --tx-body-file ${DEVNET_DIR}/seed-${ACTOR}.draft \
        --signing-key-file ${DEVNET_DIR}/credentials/faucet.sk \
        --out-file ${DEVNET_DIR}/seed-${ACTOR}.signed >&2
    SEED_TXID=$(ccli_ transaction txid --tx-file ${DEVNET_DIR}/seed-${ACTOR}.signed | tr -d '\r')
    SEED_TXIN="${SEED_TXID}#0"
    ccli transaction submit --tx-file ${DEVNET_DIR}/seed-${ACTOR}.signed >&2

    echo -n >&2 "Waiting for utxo ${SEED_TXIN}.."

    while [[ "$(ccli query utxo --tx-in "${SEED_TXIN}" --out-file /dev/stdout | jq ".\"${SEED_TXIN}\"")" = "null" ]]; do
        sleep 1
        echo -n >&2 "."
    done
    echo >&2 "Done"
}

function publishReferenceScripts() {
  echo >&2 "Publishing reference scripts..."
  hnode publish-scripts \
    --testnet-magic ${NETWORK_ID} \
    --node-socket ${DEVNET_DIR}/node.socket \
    --cardano-signing-key devnet/credentials/faucet.sk
}

echo >&2 "Fueling up hydra nodes of alice, bob and carol..."
seedFaucet "alice" 20000000 # 20 Ada fuel available to the node
seedFaucet "bob" 20000000 # 20 Ada fuel available to the node
seedFaucet "carol" 20000000 # 20 Ada fuel available to the node
echo >&2 "Distributing funds to alice, bob and carol..."
seedFaucet "alice-funds" 100000000 # 100 Ada to commit
seedFaucet "bob-funds" 50000000 # 50 Ada to commit
seedFaucet "carol-funds" 25000000 # 25 Ada to commit
echo "HYDRA_SCRIPTS_TX_ID=$(publishReferenceScripts)" > .env
echo >&2 "Environment variable stored in '.env'"
echo >&2 -e "\n\t$(cat .env)\n"
