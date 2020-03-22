module SkyblockHelper.Gen
open SkyblockHelper
// SkyblockHelper.Resources+CompactableState
let CompactableStateCases =
  [
    "Part", Resources.CompactableState.Part
    "Compacted", Resources.CompactableState.Compacted
  ]
// SkyblockHelper.Resources+IronState
let IronStateCases =
  [
    "Ore", Resources.IronState.Ore
    "Ingot", Resources.IronState.Ingot
    "Block", Resources.IronState.Block
    "Enchanted", Resources.IronState.Enchanted
    "EnchantedBlock", Resources.IronState.EnchantedBlock
  ]
// SkyblockHelper.Resources+Resource
let ResourceCases =
  [
    "Wheat", Resources.Resource.Wheat
    "Seeds", Resources.Resource.Seeds
    "RawBeef", Resources.Resource.RawBeef
    "Watermelon", Resources.Resource.Watermelon
    "Pumpkin", Resources.Resource.Pumpkin
    "Cobblestone", Resources.Resource.Cobblestone
    "Coal", Resources.Resource.Coal
    "Iron", Resources.Resource.Iron
    "Gold", Resources.Resource.Gold
    "Lapis", Resources.Resource.Lapis
    "Emerald", Resources.Resource.Emerald
    "Diamond", Resources.Resource.Diamond
  ]