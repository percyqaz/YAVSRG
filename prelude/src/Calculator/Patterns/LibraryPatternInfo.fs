namespace Prelude.Calculator.Patterns

open Prelude
open Prelude.Charts
open Prelude.Calculator
open Prelude.Calculator.Patterns

type LibraryCategoryInfo =
    {
        Amount: Time
        Difficulty: float32 * float32
        Importance: float32

        SpecificBPMs: (int<beat / minute / rate> * Time * (float32 * float32)) array
    }

type LibraryUncategorisedInfo =
    {
        Amount: Time
        Difficulty: float32 * float32
        Importance: float32
    }

/// Precalculated and stored for every chart
/// Enough data to find similarities for endless mode, for groupings, etc
/// Data is general enough to estimate similarities and difficulties on rates
type LibraryPatternInfo =
    {
        Difficulty: float32 * float32

        SVAmount: Time
        HoldNotePercent: float32
        Purity: float32

        Jacks: LibraryCategoryInfo
        Chordstream: LibraryCategoryInfo
        Stream: LibraryCategoryInfo
        Uncategorised: LibraryUncategorisedInfo

        Tags: Set<ChartTag>
    }

    static member Default =
        {
            Difficulty = 0.0f, 0.0f
            SVAmount = 0.0f<ms>
            HoldNotePercent = 0.0f
            Purity = 0.0f
            Jacks = { Amount = 0.0f<ms>; Difficulty = 0.0f, 0.0f; Importance = 0.0f; SpecificBPMs = [||] }
            Chordstream = { Amount = 0.0f<ms>; Difficulty = 0.0f, 0.0f; Importance = 0.0f; SpecificBPMs = [||] }
            Stream = { Amount = 0.0f<ms>; Difficulty = 0.0f, 0.0f; Importance = 0.0f; SpecificBPMs = [||] }
            Uncategorised = { Amount = 0.0f<ms>; Difficulty = 0.0f, 0.0f; Importance = 0.0f }
            Tags = Set.empty
        }

    member this.EstimatedDifficulty (rate: Rate) : float32 =
        let difficulty100, difficulty150 = this.Difficulty
        difficulty100 + (difficulty150 - difficulty100) * (rate - 1.0f<rate>) * 2.0f</rate>

    member this.EstimatedRate (difficulty: float32) : Rate =
        let difficulty100, difficulty150 = this.Difficulty
        let d = (difficulty - difficulty100) / (difficulty150 - difficulty100)
        1.0f<rate> + d * 0.5f<rate>

    member this.JackBPM =
        if this.Tags.Contains ChartTag.Jacks then
            this.Jacks.SpecificBPMs |> Array.tryHead |> Option.map (fun (bpm, _, _) -> bpm)
        else
            None

    member this.ChordstreamBPM =
        if this.Tags.Contains ChartTag.Chordstream then
            this.Chordstream.SpecificBPMs |> Array.tryHead |> Option.map (fun (bpm, _, _) -> bpm)
        else
            None

    member this.StreamBPM =
        if this.Tags.Contains ChartTag.Stream then
            this.Stream.SpecificBPMs |> Array.tryHead |> Option.map (fun (bpm, _, _) -> bpm)
        else
            None

module LibraryPatternInfo =

    let category_info (segments: Segment<float32 * float32> array) : LibraryCategoryInfo =

        let duration = segments |> Seq.sumBy (fun s -> s.End - s.Start)
        let rating100 = segments |> Seq.map _.Contents |> Seq.concat |> Seq.map _.Strains |> Seq.concat |> Seq.map fst |> Seq.filter (fun x -> x > 0.0f) |> Difficulty.weighted_overall_difficulty
        let rating150 = segments |> Seq.map _.Contents |> Seq.concat |> Seq.map _.Strains |> Seq.concat |> Seq.map snd |> Seq.filter (fun x -> x > 0.0f) |> Difficulty.weighted_overall_difficulty
        let overall_importance = importance duration rating100

        let bpm_groups =
            segments
            |> Seq.groupBy (fun s -> s.Type.BPM)
            |> Seq.map (fun (bpm, segs) ->
                let duration = segs |> Seq.sumBy (fun s -> s.End - s.Start)
                let rating100 = segs |> Seq.map _.Contents |> Seq.concat |> Seq.map _.Strains |> Seq.concat |> Seq.map fst |> Seq.filter (fun x -> x > 0.0f) |> Difficulty.weighted_overall_difficulty
                let rating150 = segs |> Seq.map _.Contents |> Seq.concat |> Seq.map _.Strains |> Seq.concat |> Seq.map snd |> Seq.filter (fun x -> x > 0.0f) |> Difficulty.weighted_overall_difficulty
                (bpm, duration, (rating100, rating150)), importance duration rating100
            )
            |> Seq.sortByDescending snd
            |> Seq.filter (fun ((_, _, _), imp) -> imp > overall_importance * RELATIVE_IMPORTANCE_THRESHOLD)
            |> Seq.map fst
            |> Seq.toArray

        {
            Amount = duration
            Difficulty = rating100, rating150
            Importance = overall_importance
            SpecificBPMs = bpm_groups
        }

    let uncategorised_info (segments: Segment<float32 * float32> array) : LibraryUncategorisedInfo =

        let duration = segments |> Seq.sumBy (fun s -> s.End - s.Start)
        let rating100 = segments |> Seq.map _.Contents |> Seq.concat |> Seq.map _.Strains |> Seq.concat |> Seq.map fst |> Seq.filter (fun x -> x > 0.0f) |> Difficulty.weighted_overall_difficulty
        let rating150 = segments |> Seq.map _.Contents |> Seq.concat |> Seq.map _.Strains |> Seq.concat |> Seq.map snd |> Seq.filter (fun x -> x > 0.0f) |> Difficulty.weighted_overall_difficulty
        let overall_importance = importance duration rating100

        {
            Amount = duration
            Difficulty = rating100, rating150
            Importance = overall_importance
        }

    let from_chart (chart: Chart) : LibraryPatternInfo =
        let primitives = Primitives.calculate_multirate chart

        let rating100 =
            primitives
            |> Seq.map _.Strains
            |> Seq.concat
            |> Seq.map fst
            |> Seq.filter (fun x -> x > 0.0f)
            |> Difficulty.weighted_overall_difficulty

        let rating150 =
            primitives
            |> Seq.map _.Strains
            |> Seq.concat
            |> Seq.map snd
            |> Seq.filter (fun x -> x > 0.0f)
            |> Difficulty.weighted_overall_difficulty

        let segments = CorePatternParser.parse primitives |> Seq.map CorePatternParser.make_segment |> Array.ofSeq

        let jacks = segments |> Seq.filter (fun s -> s.Type.IsJacks) |> Array.ofSeq |> category_info
        let chordstream = segments |> Seq.filter (fun s -> s.Type.IsChordstream) |> Array.ofSeq |> category_info
        let stream = segments |> Seq.filter (fun s -> s.Type.IsStream) |> Array.ofSeq |> category_info
        let uncategorised = segments |> Seq.filter (fun s -> s.Type.IsUncategorized) |> Array.ofSeq |> uncategorised_info

        let total = jacks.Importance + chordstream.Importance + stream.Importance + uncategorised.Importance
        let amt_jack = jacks.Importance / total
        let amt_chordstream = chordstream.Importance / total
        let amt_stream = stream.Importance / total
        let amt_other = uncategorised.Importance / total

        let sv_time = Metrics.sv_time chart
        let ln_percent = Metrics.ln_percent chart

        let tags : Set<ChartTag> =
            seq {
                if amt_jack > MAJORITY_THRESHOLD then
                    yield ChartTag.Jacks

                    match Array.tryHead jacks.SpecificBPMs with
                    | Some (_, amount, (rating100, _)) when importance amount rating100 / total > PURITY_THRESHOLD -> yield ChartTag.Pure
                    | _ -> ()
                elif amt_chordstream > MAJORITY_THRESHOLD then
                    yield ChartTag.Chordstream

                    match Array.tryHead chordstream.SpecificBPMs with
                    | Some (_, amount, (rating100, _)) when importance amount rating100 / total > PURITY_THRESHOLD -> yield ChartTag.Pure
                    | _ -> ()
                elif amt_stream > MAJORITY_THRESHOLD then
                    yield ChartTag.Stream

                    match Array.tryHead stream.SpecificBPMs with
                    | Some (_, amount, (rating100, _)) when importance amount rating100 / total > PURITY_THRESHOLD -> yield ChartTag.Pure
                    | _ -> ()

                elif amt_other > MAJORITY_THRESHOLD then
                    if fst uncategorised.Difficulty < 4.0f then yield ChartTag.Beginner else yield ChartTag.Hybrid

                elif amt_stream + amt_other > MAJORITY_THRESHOLD && amt_jack + amt_other < MAJORITY_THRESHOLD then
                    yield ChartTag.Stream

                elif amt_chordstream > amt_stream && amt_chordstream + amt_stream > MAJORITY_THRESHOLD && amt_jack + amt_other < MAJORITY_THRESHOLD then
                    yield ChartTag.Chordstream

                else
                    yield ChartTag.Hybrid

                if sv_time > SV_AMOUNT_THRESHOLD then
                    yield ChartTag.SV
                if ln_percent > LN_AMOUNT_THRESHOLD then
                    yield ChartTag.LN
            }
            |> Set.ofSeq

        {
            Difficulty = rating100, rating150
            SVAmount = sv_time
            HoldNotePercent = ln_percent
            Purity = 0.0f

            Jacks = jacks
            Chordstream = chordstream
            Stream = stream
            Uncategorised = uncategorised

            Tags = tags
        }