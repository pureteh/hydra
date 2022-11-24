import { Lucid } from "https://unpkg.com/lucid-cardano@0.7.8/web/mod.js"

const HydraProvider = {
  getProtocolParameters: () => {
    console.log("getProtocolParameters");
    return {
      minFeeA: 0, minFeeB: 0, maxTxSize: 0, maxValSize: 0,
      keyDeposit: BigInt(0),
      poolDeposit: BigInt(0),
      priceMem: 0, priceStep: 0,
      maxTxExMem: BigInt(0),
      maxTxExSteps: BigInt(0),
      coinsPerUtxoByte: BigInt(0),
      collateralPercentage: 0,
      maxCollateralInputs: 0,
      costModels: {
        PlutusV1: [],
        PlutusV2: []
      }
    }
  }
}

// NOTE: The network is only used for slot config / time stuff.
const lucid = await Lucid.new(HydraProvider, "Preview");

export const paintPixel = async (getUTxO, x, y, color) => {
  console.log("should paint pixel: ", x, y, color);

  const utxo = await getUTxO();
  console.log("got utxo", utxo);

  // FIXME: hardocded
  const addr = "addr_test1vrgxw5dfjk4khlep3auxrw7tp233jqsezagjusj6fsf6myq32u949"

  let ownUtxo = null;
  for (const [key, value] of Object.entries(utxos)) {
    if (value.address === addr) {
      const ownTxIn = key.split("#");
      ownUtxo = {
        txHash: ownTxIn[0],
        outputIndex: parseInt(ownTxIn[1]),
        assets: value.value, // NOTE: maybe wrap into BigInt(value.value). WARN: There are many potentail values
        address: value.address
      }
      return;
    }
  }

  lucid.newTx().collectFrom([utxo]).complete()
}

