import { Lucid } from "https://unpkg.com/lucid-cardano@0.7.8/web/mod.js"


export const paintPixel = async (x, y, color) => {
  console.log("should paint pixel: ", x, y, color);
  const lucid = await Lucid.new({}, "Hydra");
  console.log(lucid);
}
