module AppDomain.Collections.Pets

open Shared.Helpers
open SkyblockHelper

// let getXpTo100 =
//     function
//     | Common -> 5_624_785
//     | Uncommon -> 8_644_220
//     | Rare -> 12_626_665
//     | Epic -> 18_608_500
//     | Legendary -> 25_353_2300

type Candy =
    | Simple
    | Great
    | Superb
// no Enchanting pets

type PetType =
    | Farming
    | Mining
    | Combat
    | Foraging
    | Fishing
    | Alchemy
    with
        static member Humanize : PetType -> string = string
        static member All = [
            Farming
            Mining
            Combat
            Foraging
            Fishing
            Alchemy
        ]
        static member Parse x =
            PetType.All
            |> List.tryPick(flip equalsIStr x)
        // static member Parse =
        //     function
        //     | EqualsIStr Farming x -> x
        //     | EqualsI (string Mining) -> Mining
        //     | EqualsI (string Combat) -> Combat
        //     | EqualsI (string Foraging) -> Foraging


let getLvlXp =
    fun lvl -> lvl + 1
    >> min 100
    >> max 2
    >> function
    | 2 -> [100;175;275;440;660]
    | 3 -> [ 110;190;300;490;730 ]
    | 4 -> [ 120;210;330;540;800 ]
    | 5 -> [ 130;230;360;600;880 ]
    | 6 -> [ 145;250;400;660;960 ]
    | 7 -> [ 160;275;440;730;1_050 ]
    | 8 -> [ 175;300;490;800;1_150 ]
    | 9 -> [ 190;330;540;880;1_260 ]
    | 10 -> [ 210;360;600;960;1_380 ]
    | 11 -> [ 230;400;660;1_050;1_510 ]
    | 12 -> [ 250;440;730;1_150;1_650 ]
    | 13 -> [ 275;490;800;1_260;1_800 ]
    | 14 -> [ 300;540;880;1_380;1_960 ]
    | 15 -> [ 330;600;960;1_510;2_130 ]
    | 16 -> [ 360;660;1_050;1_650;2_310 ]
    | 17 -> [ 400;730;1_150;1_800;2_500 ]
    | 18 -> [ 440;800;1_260;1_960;2_700 ]
    | 19 -> [ 490;880;1_380;2_130;2_920 ]
    | 20 -> [ 540;960;1_510;2_310;3_160 ]
    | 21 -> [ 600;1_050;1_650;2_500;3_420 ]
    | 22 -> [ 660;1_150;1_800;2_700;3_700 ]
    | 23 -> [ 730;1_260;1_960;2_920;4_000 ]
    | 24 -> [ 800;1_380;2_130;3_160;4_350 ]
    | 25 -> [ 880;1_510;2_310;3_420;4_750 ]
    | 26 -> [ 960;1_650;2_500;3_700;5_200 ]
    | 27 -> [ 1_050;1_800;2_700;4_000;5_700 ]
    | 28 -> [ 1_150;1_960;2_920;4_350;6_300 ]
    | 29 -> [ 1_260;2_130;3_160;4_750;7_000 ]
    | 30 -> [ 1_380;2_310;3_420;5_200;7_800 ]
    | 31 -> [ 1_510;2_500;3_700;5_700;8_700 ]
    | 32 -> [ 1_650;2_700;4_000;6_300;9_700 ]
    | 33 -> [ 1_800;2_920;4_350;7_000;10_800 ]
    | 34 -> [ 1_960;3_160;4_750;7_800;12_000 ]
    | 35 -> [ 2_130;3_420;5_200;8_700;13_300 ]
    | 36 -> [ 2_310;3_700;5_700;9_700;14_700 ]
    | 37 -> [ 2_500;4_000;6_300;10_800;16_200 ]
    | 38 -> [ 2_700;4_350;7_000;12_000;17_800 ]
    | 39 -> [ 2_920;4_750;7_800;13_300;19_500 ]
    | 40 -> [ 3_160;5_200;8_700;14_700;21_300 ]
    | 41 -> [ 3_420;5_700;9_700;16_200;23_200 ]
    | 42 -> [ 3_700;6_300;10_800;17_800;25_200 ]
    | 43 -> [ 4_000;7_000;12_000;19_500;27_400 ]
    | 44 -> [ 4_350;7_800;13_300;21_300;29_800 ]
    | 45 -> [ 4_750;8_700;14_700;23_200;32_400 ]
    | 46 -> [ 5_200;9_700;16_200;25_200;35_200 ]
    | 47 -> [ 5_700;10_800;17_800;27_400;38_200 ]
    | 48 -> [ 6_300;12_000;19_500;29_800;41_400 ]
    | 49 -> [ 7_000;13_300;21_300;32_400;44_800 ]
    | 50 -> [ 7_800;14_700;23_200;35_200;48_400 ]
    | 51 -> [ 8_700;16_200;25_200;38_200;52_200 ]
    | 52 -> [ 9_700;17_800;27_400;41_400;56_200 ]
    | 53 -> [ 10_800;19_500;29_800;44_800;60_400 ]
    | 54 -> [ 12_000;21_300;32_400;48_400;64_800 ]
    | 55 -> [ 13_300;23_200;35_200;52_200;69_400 ]
    | 56 -> [ 14_700;25_200;38_200;56_200;74_200 ]
    | 57 -> [ 16_200;27_400;41_400;60_400;79_200 ]
    | 58 -> [ 17_800;29_800;44_800;64_800;84_700 ]
    | 59 -> [ 19_500;32_400;48_400;69_400;90_700 ]
    | 60 -> [ 21_300;35_200;52_200;74_200;97_200 ]
    | 61 -> [ 23_200;38_200;56_200;79_200;104_200 ]
    | 62 -> [ 25_200;41_400;60_400;84_700;111_700 ]
    | 63 -> [ 27_400;44_800;64_800;90_700;119_700 ]
    | 64 -> [ 29_800;48_400;69_400;97_200;128_200 ]
    | 65 -> [ 32_400;52_200;74_200;104_200;137_200 ]
    | 66 -> [ 35_200;56_200;79_200;111_700;146_700 ]
    | 67 -> [ 38_200;60_400;84_700;119_700;156_700 ]
    | 68 -> [ 41_400;64_800;90_700;128_200;167_700 ]
    | 69 -> [ 44_800;69_400;97_200;137_200;179_700 ]
    | 70 -> [ 48_400;74_200;104_200;146_700;192_700 ]
    | 71 -> [ 52_200;79_200;111_700;156_700;206_700 ]
    | 72 -> [ 56_200;84_700;119_700;167_700;221_700 ]
    | 73 -> [ 60_400;90_700;128_200;179_700;237_700 ]
    | 74 -> [ 64_800;97_200;137_200;192_700;254_700 ]
    | 75 -> [ 69_400;104_200;146_700;206_700;272_700 ]
    | 76 -> [ 74_200;111_700;156_700;221_700;291_700 ]
    | 77 -> [ 79_200;119_700;167_700;237_700;311_700 ]
    | 78 -> [ 84_700;128_200;179_700;254_700;333_700 ]
    | 79 -> [ 90_700;137_200;192_700;272_700;357_700 ]
    | 80 -> [ 97_200;146_700;206_700;291_700;383_700 ]
    | 81 -> [ 104_200;156_700;221_700;311_700;411_700 ]
    | 82 -> [ 111_700;167_700;237_700;333_700;441_700 ]
    | 83 -> [ 119_700;179_700;254_700;357_700;476_700 ]
    | 84 -> [ 128_200;192_700;272_700;383_700;516_700 ]
    | 85 -> [ 137_200;206_700;291_700;411_700;561_700 ]
    | 86 -> [ 146_700;221_700;311_700;441_700;611_700 ]
    | 87 -> [ 156_700;237_700;333_700;476_700;666_700 ]
    | 88 -> [ 167_700;254_700;357_700;516_700;726_700 ]
    | 89 -> [ 179_700;272_700;383_700;561_700;791_700 ]
    | 90 -> [ 192_700;291_700;411_700;611_700;861_700 ]
    | 91 -> [ 206_700;311_700;441_700;666_700;936_700 ]
    | 92 -> [ 221_700;333_700;476_700;726_700;1_016_700 ]
    | 93 -> [ 237_700;357_700;516_700;791_700;1_101_700 ]
    | 94 -> [ 254_700;383_700;561_700;861_700;1_191_700 ]
    | 95 -> [ 272_700;411_700;611_700;936_700;1_286_700 ]
    | 96 -> [ 291_700;441_700;666_700;1_016_700;1_386_700 ]
    | 97 -> [ 311_700;476_700;726_700;1_101_700;1_496_700 ]
    | 98 -> [ 333_700;516_700;791_700;1_191_700;1_616_700 ]
    | 99 -> [ 357_700;561_700;861_700;1_286_700;1_746_700 ]
    | 100 -> [ 383_700;611_700;936_700;1_386_700;1_886_700 ]
    | x -> failwithf "Invalid level target %i" x
    >> (fun lvls r ->
        match r with
        | Rarity.Common -> lvls.[0]
        | Uncommon -> lvls.[1]
        | Rare -> lvls.[2]
        | Epic -> lvls.[3]
        | Legendary -> lvls.[4]
    )