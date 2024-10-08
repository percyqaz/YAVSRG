﻿namespace Prelude.Data.Library.Endless

open System
open Percyqaz.Common
open Prelude
open Prelude.Gameplay
open Prelude.Gameplay.Mods
open Prelude.Charts.Processing.Patterns
open Prelude.Data.Library
open Prelude.Data.Library.Caching
open Prelude.Data.Library.Sorting
open Prelude.Data.Library.Collections
open Prelude.Data

[<RequireQualifiedAccess>]
type Variety =
    | Low
    | High

type SuggestionContext =
    {
        BaseChart: CachedChart * float32
        Mods: ModState
        Filter: Filter
        MinimumRate: float32
        MaximumRate: float32
        RulesetId: string
        Ruleset: Ruleset
        Library: Library
        ScoreDatabase: ScoreDatabase
    }
    member this.LibraryViewContext: LibraryViewContext =
        {
            Rate = let (_, rate) = this.BaseChart in rate
            RulesetId = this.RulesetId
            Ruleset = this.Ruleset
            Library = this.Library
            ScoreDatabase = this.ScoreDatabase
        }

module Suggestion =

    let mutable recommended_already = Set.empty

    let most_common_pattern (total: ScaledTime) (patterns: PatternInfo) =
        if patterns.Patterns = [] then Stream else
        patterns.Patterns
        |> Seq.groupBy _.Pattern
        |> Seq.map (fun (p, ps) -> p, Seq.sumBy (fun (p: PatternSummary.PatternBreakdown) -> p.Amount) ps)
        |> Seq.maxBy snd
        |> fst

    let private pattern_similarity (total: ScaledTime) (rate: float32, patterns: PatternInfo) (c_rate: float32, c_patterns: PatternInfo) : float32 =


        let c_total = c_patterns.Patterns |> Seq.sumBy _.Amount
        if most_common_pattern total patterns <> most_common_pattern c_total c_patterns then 0.0f
        else

        let mutable similarity = 0.0f
        for p2 in c_patterns.Patterns do
            for p1 in patterns.Patterns do
                if p1.Pattern = p2.Pattern then
                    let mixed_similarity = if p1.Mixed = p2.Mixed then 1.0f else 0.5f
                    let bpm_similarity =
                        let difference = (rate * float32 p1.BPM) / (c_rate * float32 p2.BPM) |> log |> abs
                        Math.Clamp(1.0f - 10.0f * difference, 0.0f, 1.0f)
                    let density_similarity =
                        let difference = (rate * p1.Density75) / (c_rate * p2.Density75) |> log |> abs
                        Math.Clamp(1.0f - 10.0f * difference, 0.0f, 1.0f)
                    similarity <- similarity + mixed_similarity * bpm_similarity * density_similarity * (p1.Amount / total) * (p2.Amount / c_total)
        similarity

    let get_random (filter_by: Filter) (ctx: LibraryViewContext) : CachedChart option =
        let rand = Random()

        let charts =
            Filter.apply_seq (filter_by, ctx) ctx.Library.Cache.Entries.Values
            |> Array.ofSeq

        if charts.Length > 0 then
            let result = charts.[rand.Next charts.Length]
            Some result
        else
            None

    let private get_core_suggestions (ctx: SuggestionContext) : (CachedChart * float32) seq =

        let base_chart, rate = ctx.BaseChart
        
        match Cache.patterns_by_hash base_chart.Hash ctx.Library.Cache with
        | None -> Seq.empty
        | Some patterns ->

        recommended_already <- Set.add base_chart.Hash recommended_already
        recommended_already <- Set.add (base_chart.Title.ToLower()) recommended_already

        let target_density = patterns.Density50 * rate

        let max_ln_pc = patterns.LNPercent + 0.1f
        let min_ln_pc = patterns.LNPercent - 0.1f

        let now = Timestamp.now ()
        let THIRTY_DAYS = 30L * 24L * 3600_000L

        let candidates =
            ctx.Library.Cache.Entries.Values
            |> Seq.filter (fun cc -> cc.Keys = base_chart.Keys)
            |> Seq.filter (fun cc -> not (recommended_already.Contains cc.Hash))
            |> Seq.filter (fun cc -> not (recommended_already.Contains (cc.Title.ToLower())))
            |> Seq.choose (fun cc -> 
                match Cache.patterns_by_hash cc.Hash ctx.Library.Cache with 
                | None -> None
                | Some p -> 
                    let best_rate = target_density / p.Density50
                    let best_approx_rate = round(best_rate / 0.05f) * 0.05f
                    if best_approx_rate >= ctx.MinimumRate && best_approx_rate <= ctx.MaximumRate then
                        Some (cc, (best_approx_rate, p))
                    else None
            )
            |> Seq.filter (fun (cc, (rate, p)) -> p.LNPercent >= min_ln_pc && p.LNPercent <= max_ln_pc)
            |> Seq.filter (fun (cc, (rate, p)) -> now - (ScoreDatabase.get cc.Hash ctx.ScoreDatabase).LastPlayed > THIRTY_DAYS)
            |> Filter.apply_ctx_seq (ctx.Filter, ctx.LibraryViewContext)

        let total_pattern_amount = patterns.Patterns |> Seq.sumBy _.Amount
        let spikiness = patterns.Density90 / patterns.Density50

        seq {
            for cc, (c_rate, c_patterns) in candidates do

                let sv_compatibility = 
                    if (patterns.SVAmount < 30000.0f<ms>) <> (c_patterns.SVAmount < 30000.0f<ms>) then
                        0.5f
                    else 1.0f

                let length_compatibility =
                    let l1 = base_chart.Length / rate
                    let l2 = cc.Length / c_rate
                    1.0f - min 1.0f (abs (l2 - l1) / l1 * 10.0f)

                let difficulty_compatibility =
                    let c_spikiness = c_patterns.Density90 / c_patterns.Density50
                    1.0f - min 1.0f (abs (c_spikiness - spikiness) * 10.0f)

                let pattern_compatibility =
                    pattern_similarity total_pattern_amount (rate, patterns) (c_rate, c_patterns)

                let compatibility = 
                    sv_compatibility * length_compatibility * difficulty_compatibility * pattern_compatibility

                yield (cc, c_rate), compatibility
        }
        |> Seq.sortByDescending snd
        |> Seq.map fst

    let get_suggestion (ctx: SuggestionContext) : (CachedChart * float32) option =
        let rand = Random()
        let best_matches = get_core_suggestions ctx |> Seq.truncate 50 |> Array.ofSeq

        if best_matches.Length = 0 then
            None
        else

            let cc, rate =
                let index =
                    rand.NextDouble()
                    |> fun x -> x * x
                    |> fun x -> x * float best_matches.Length
                    |> floor
                    |> int

                best_matches.[index]

            recommended_already <- Set.add cc.Hash recommended_already
            recommended_already <- Set.add (cc.Title.ToLower()) recommended_already
            
            Some (cc, rate)

type EndlessModeState =
    internal {
        mutable Queue: (CachedChart * PlaylistEntryInfo) list
    }

module EndlessModeState =

    let create () = { Queue = [] }

    let private shuffle_playlist_charts (items: 'T seq) =
        let random = new Random()
        items |> Seq.map (fun x -> x, random.Next()) |> Seq.sortBy snd |> Seq.map fst

    let queue_playlist (from: int) (playlist: Playlist) (library: Library) (state: EndlessModeState) =
        state.Queue <-
            playlist.Charts
            |> Seq.skip from
            |> Seq.choose (fun (c, info) ->
                match Cache.by_hash c.Hash library.Cache with
                | Some cc -> Some(cc, info)
                | None -> None
            )
            |> List.ofSeq

    let queue_shuffled_playlist (playlist: Playlist) (library: Library) (state: EndlessModeState) =
        state.Queue <-
            playlist.Charts
            |> Seq.choose (fun (c, info) ->
                match Cache.by_hash c.Hash library.Cache with
                | Some cc -> Some(cc, info)
                | None -> None
            )
            |> shuffle_playlist_charts
            |> List.ofSeq

    let clear_queue (state: EndlessModeState) =
        state.Queue <- []

    type Next =
        {
            Chart: CachedChart
            Rate: float32
            Mods: ModState
            NextContext: SuggestionContext
        }

    let next (ctx: SuggestionContext) (state: EndlessModeState) : Next option =
        match state.Queue with
        | (chart, { Rate = rate; Mods = mods }) :: xs ->
            state.Queue <- xs
            Some
                {
                    Chart = chart
                    Rate = rate.Value
                    Mods = mods.Value
                    NextContext = ctx
                }
        | [] ->
            match Suggestion.get_suggestion ctx with
            | Some (next_cc, rate) ->
                Some
                    {
                        Chart = next_cc
                        Rate = rate
                        Mods = ctx.Mods
                        NextContext = ctx
                    }
            | None -> None