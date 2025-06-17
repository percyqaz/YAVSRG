namespace Prelude.Data.Library.Endless

open System
open Percyqaz.Common
open Prelude
open Prelude.Mods
open Prelude.Calculator.Patterns
open Prelude.Data.Library
open Prelude.Data.User

module Suggestion =

    let mutable recommended_already = Set.empty

    let get_random (filter_by: Filter) (ctx: LibraryViewContext) : ChartMeta option =
        let rand = Random()

        let charts =
            filter_by.Apply ctx.Library.Charts.Cache.Values
            |> Array.ofSeq

        if charts.Length > 0 then
            let result = charts.[rand.Next charts.Length]
            Some result
        else
            None

    let internal get_core_suggestions (ctx: SuggestionContext) : (ChartMeta * Rate) seq =

        let base_chart, rate = ctx.BaseChart

        let patterns = base_chart.Patterns

        recommended_already <- Set.add base_chart.Hash recommended_already
        recommended_already <- Set.add (base_chart.Title.ToLower()) recommended_already

        let target_difficulty = base_chart.Patterns.EstimatedDifficulty(rate)

        let max_ln_pc = patterns.HoldNotePercent + 0.1f
        let min_ln_pc = patterns.HoldNotePercent - 0.1f

        let now = Timestamp.now ()
        let THIRTY_DAYS = 30L * 24L * 3600_000L

        let candidates =
            ctx.Library.Charts.Cache.Values
            |> Seq.filter (fun chart_meta -> chart_meta.Keys = base_chart.Keys)
            |> Seq.filter (fun chart_meta -> not (recommended_already.Contains chart_meta.Hash))
            |> Seq.filter (fun chart_meta -> not (recommended_already.Contains (chart_meta.Title.ToLower())))
            |> Seq.choose (fun chart_meta ->
                let best_rate = chart_meta.Patterns.EstimatedRate(target_difficulty)
                let best_approx_rate = round(best_rate / 0.05f<rate>) * 0.05f<rate>
                if best_approx_rate >= ctx.MinimumRate && best_approx_rate <= ctx.MaximumRate then
                    Some (chart_meta, best_approx_rate)
                else None
            )
            |> Seq.filter (fun (chart_meta, _) -> chart_meta.Patterns.HoldNotePercent >= min_ln_pc && chart_meta.Patterns.HoldNotePercent <= max_ln_pc)
            |> if ctx.OnlyNewCharts then
                Seq.filter (fun (chart_meta, _) -> now - (UserDatabase.get_chart_data chart_meta.Hash ctx.UserDatabase).LastPlayed > THIRTY_DAYS)
               else
                id
            |> ctx.Filter.Apply

        //let total_pattern_amount = patterns.Clusters |> Seq.sumBy _.Amount
        //let spikiness = patterns.Density90 / patterns.Density50

        seq {
            for chart_meta, c_rate in candidates do

                let sv_compatibility =
                    if (patterns.SVAmount < 30000.0f<ms>) <> (chart_meta.Patterns.SVAmount < 30000.0f<ms>) then
                        0.5f
                    else 1.0f

                let length_compatibility =
                    let l1 = base_chart.Length / rate
                    let l2 = chart_meta.Length / c_rate
                    1.0f - min 1.0f (abs (l2 - l1) / l1 * 10.0f)

                let difficulty_compatibility = 1.0f
                    //let c_spikiness = c_patterns.Density90 / c_patterns.Density50
                    //1.0f - min 1.0f (abs (c_spikiness - spikiness) * 10.0f)

                let pattern_compatibility = Similarity.calculate patterns chart_meta.Patterns

                let compatibility =
                    sv_compatibility * length_compatibility * difficulty_compatibility * pattern_compatibility

                yield (chart_meta, c_rate), compatibility
        }
        |> Seq.sortByDescending snd
        |> Seq.map fst

    let get_suggestion (ctx: SuggestionContext) : (ChartMeta * Rate) option =
        let rand = Random()
        let best_matches = get_core_suggestions ctx |> Seq.truncate 50 |> Array.ofSeq

        if best_matches.Length = 0 then
            None
        else

            let chart_meta, rate =
                let index =
                    rand.NextDouble()
                    |> fun x -> x * x
                    |> fun x -> x * float best_matches.Length
                    |> floor
                    |> int

                best_matches.[index]

            recommended_already <- Set.add chart_meta.Hash recommended_already
            recommended_already <- Set.add (chart_meta.Title.ToLower()) recommended_already

            Some (chart_meta, rate)

type EndlessModeState =
    internal {
        mutable Playlist: string
        mutable Queue: (ChartMeta * (int * PlaylistEntryInfo)) list
    }

module EndlessModeState =

    let create () = { Playlist = ""; Queue = [] }

    let private shuffle_playlist_charts (items: 'T seq) =
        let random = new Random()
        items |> Seq.map (fun x -> x, random.Next()) |> Seq.sortBy snd |> Seq.map fst

    let queue_playlist (from: int) (name: string) (playlist: Playlist) (library: Library) (filter: FilteredSearch) (state: EndlessModeState) =
        state.Playlist <- name
        state.Queue <-
            playlist.Charts
            |> Seq.indexed
            |> Seq.skip from
            |> Seq.choose (fun (i, (c, info)) ->
                match ChartDatabase.get_meta c.Hash library.Charts with
                | Some chart_meta -> Some(chart_meta, (i, info))
                | None -> None
            )
            |> filter.Apply
            |> List.ofSeq

    let queue_shuffled_playlist (name: string) (playlist: Playlist) (library: Library) (filter: FilteredSearch) (state: EndlessModeState) =
        state.Playlist <- name
        state.Queue <-
            playlist.Charts
            |> Seq.indexed
            |> Seq.choose (fun (i, (c, info)) ->
                match ChartDatabase.get_meta c.Hash library.Charts with
                | Some chart_meta -> Some(chart_meta, (i, info))
                | None -> None
            )
            |> filter.Apply
            |> shuffle_playlist_charts
            |> List.ofSeq

    let clear_queue (state: EndlessModeState) =
        state.Playlist <- ""
        state.Queue <- []

    type Next =
        {
            Chart: ChartMeta
            Rate: Rate
            Mods: ModState
            LibraryContext: LibraryContext
            NextContext: SuggestionContext
        }

    let next (ctx: SuggestionContext) (state: EndlessModeState) : Next option =
        match state.Queue with
        | (chart, (index, playlist_data)) :: xs ->
            state.Queue <- xs
            Some
                {
                    Chart = chart
                    Rate = playlist_data.Rate.Value
                    Mods = playlist_data.Mods.Value
                    LibraryContext = LibraryContext.Playlist(index, state.Playlist, playlist_data)
                    NextContext = ctx
                }
        | [] ->
            match Suggestion.get_suggestion ctx with
            | Some (next_chart_meta, rate) ->
                Some
                    {
                        Chart = next_chart_meta
                        Rate = rate
                        Mods = ctx.Mods
                        LibraryContext = LibraryContext.None
                        NextContext = ctx
                    }
            | None -> None