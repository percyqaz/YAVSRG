﻿namespace Interlude.Features.Import.osu

open System
open System.IO
open Percyqaz.Common
open Prelude.Charts
open Prelude.Gameplay
open Prelude.Data.``osu!``
open Prelude.Data.Library
open Prelude.Data.User
open Interlude.Content

module Replays =

    let parse_replay_file (replay_path: string) =
        try
            use file = File.OpenRead replay_path
            use br = new BinaryReader(file)
            Some(OsuScoreDatabase_Score.Read br)
        with err ->
            Logging.Error(sprintf "Error loading replay file %s" replay_path, err)
            None

    let convert_replay_to_score (replay: OsuScoreDatabase_Score) (chart: Chart) : Result<Score, string> =
        match Mods.to_interlude_rate_and_mods replay.ModsUsed with
        | None -> Error "Invalid mods used in replay"
        | Some(rate, mods) ->

        try 
            let replay_data = decode_replay (replay, chart, rate)

            Ok {
                Timestamp =
                    DateTime.FromFileTimeUtc(replay.Timestamp).ToLocalTime()
                    |> Timestamp.from_datetime
                Replay = Replay.compress_bytes replay_data
                Rate = MathF.Round(rate, 2)
                Mods = mods
                IsImported = true
                Keys = chart.Keys
            }
        with err -> Error err.Message

    let import_replay_file (replay: OsuScoreDatabase_Score) (chart_meta: ChartMeta) (chart: Chart) : bool =
        match convert_replay_to_score replay chart with
        | Error reason -> 
            Logging.Warn(sprintf "Error importing replay: %s" reason)
            false
        | Ok score ->
            UserDatabase.delete_score chart_meta.Hash score.Timestamp Content.UserData |> ignore
            UserDatabase.save_score chart_meta.Hash score Content.UserData
            true