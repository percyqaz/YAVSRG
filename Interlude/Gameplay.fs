﻿namespace Interlude

open System
open System.Collections.Generic
open Prelude.Common
open Prelude.Charts.Interlude
open Prelude.Gameplay
open Prelude.Gameplay.Mods
open Prelude.Gameplay.Score
open Prelude.Gameplay.Difficulty
open Prelude.Gameplay.NoteColors
open Prelude.Data.ChartManager
open Prelude.Data.ScoreManager
open Interlude
open Interlude.UI
open Interlude.Utils

module Gameplay =

    let mutable internal currentChart: Chart option = None
    let mutable internal currentCachedChart: CachedChart option = None
    let mutable internal chartSaveData = None
    let mutable modifiedChart: ModChart option = None
    let mutable private coloredChart: ColorizedChart option = None
    let mutable difficultyRating: RatingReport option = None

    let mutable rate = 1.0f
    let mutable selectedMods = Map.empty
    let scores = ScoresDB()
    let cache = Cache()

    let mutable onChartUpdate = ignore
    let mutable onChartChange = ignore

    let updateChart() =
        match currentChart with
        | None -> ()
        | Some c ->
            modifiedChart <- Some <| getModChart selectedMods c
            coloredChart <- None
            difficultyRating <-
                let mc = modifiedChart.Value in
                Some <| RatingReport(mc.Notes, rate, Options.options.Playstyles.[mc.Keys - 3], mc.Keys)
            onChartUpdate()

    let changeRate amount =
        rate <- Math.Round(float (rate + amount), 2) |> float32
        Audio.changeRate rate
        updateChart()

    let changeChart (cachedChart, chart) =
        currentCachedChart <- Some cachedChart
        currentChart <- Some chart
        chartSaveData <- Some <| scores.GetOrCreateScoreData chart
        Screens.loadBackground chart.BGPath
        let localOffset = if chart.Notes.Empty then 0.0f<ms> else chartSaveData.Value.Offset.Value - offsetOf chart.Notes.First.Value
        Audio.changeTrack (chart.AudioPath, localOffset, rate)
        Audio.playFrom chart.Header.PreviewTime
        Options.options.CurrentChart.Value <- cachedChart.FilePath
        updateChart()
        onChartChange()

    let createScoreData() = createScoreData selectedMods modifiedChart.Value

    let getColoredChart() =
        match modifiedChart with
        | None -> failwith "Tried to get coloredChart when no modifiedChart exists"
        | Some mc ->
            coloredChart <- Option.defaultWith (fun () -> getColoredChart Options.options.ColorStyle.Value mc) coloredChart |> Some
            coloredChart.Value

    let makeScore (scoreData, keys) : Score = {
        time = DateTime.Now
        hitdata = compressScoreData scoreData
        rate = rate
        selectedMods = selectedMods |> ModChart.filter modifiedChart.Value
        layout = Options.options.Playstyles.[keys - 3]
        keycount = keys
    }

    let setScore (data: ScoreInfoProvider) =
        let d = chartSaveData.Value
        if
            //todo: score uploading goes here when implemented
            data.ModStatus < ModStatus.Unstored &&
            match Options.options.ScoreSaveCondition.Value with
            | _ -> true //todo: fill in this stub (pb condition will be complicated)
        then
            //add to score db
            d.Scores.Add data.Score
            scores.Save()
            //update top scores
            Options.options.Stats.TopPhysical.Apply(TopScore.add(currentCachedChart.Value.Hash, data.Score.time, data.Physical))
            Options.options.Stats.TopTechnical.Apply(TopScore.add(currentCachedChart.Value.Hash, data.Score.time, data.Technical))
            //update pbs
            let f name (target: Dictionary<string, PersonalBests<'T>>) (value: 'T) =
                if target.ContainsKey(name) then
                    let n, pb = updatePB target.[name] (value, data.Score.rate)
                    target.[name] <- n
                    pb
                else
                    target.Add(name, ((value, data.Score.rate), (value, data.Score.rate)))
                    PersonalBestType.Faster
            f data.Accuracy.Name d.Lamp data.Lamp,
            f data.Accuracy.Name d.Accuracy data.Accuracy.Value,
            //todo: maybe move this implentation to one place since it is doubled up in ScreenLevelSelect.cs
            f (data.Accuracy.Name + "|" + data.HP.Name) d.Clear (not data.HP.Failed)
        else (PersonalBestType.None, PersonalBestType.None, PersonalBestType.None)

    let save() =
        scores.Save()
        cache.Save()

    let init() =
        try
            let c, ch =
                match cache.LookupChart(Options.options.CurrentChart.Value) with
                | Some cc ->
                    match cache.LoadChart cc with
                    | Some c -> cc, c
                    | None ->
                        Logging.Error("Could not load chart file: " + cc.FilePath)
                        cache.GetGroups(K "All") (Comparison(fun _ _ -> 0)) []

                        |> fun d -> d.["All"].[0]
                        |> fun c -> c, cache.LoadChart(c).Value
                | None ->
                    Logging.Info("Could not find cached chart: " + Options.options.CurrentChart.Value)
                    cache.GetGroups(K "All") (Comparison(fun _ _ -> 0)) []
                    |> fun d -> d.["All"].[0]
                    |> fun c -> c, cache.LoadChart(c).Value
            changeChart(c, ch)
        with err ->
            Logging.Debug("Tried to auto select a chart but none exist", err)
            Screens.loadBackground ""