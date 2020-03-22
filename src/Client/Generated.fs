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
    "Melon", Resources.Resource.Melon
    "Pumpkin", Resources.Resource.Pumpkin
    "Iron", Resources.Resource.Iron
  ]