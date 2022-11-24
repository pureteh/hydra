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

export const paintPixel = async (client, x, y, color) => {
  console.log("should paint pixel: ", x, y, color);
  // NOTE: The network is only used for slot config / time stuff.
  const lucid = await Lucid.new(HydraProvider, "Preview");

  // FIXME: wait for response properly
  client.send(JSON.stringify({ tag: "GetUTxO" }))
}

