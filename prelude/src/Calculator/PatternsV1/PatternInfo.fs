namespace Prelude.Calculator.Patterns

open Prelude
open Prelude.Charts
open Prelude.Calculator

// todo: wonder about making LN sections separate to the main pattern
// 7k files like to have a big LN spam at the end that should register separately
type PatternCluster =
    {
        Pattern: CorePattern
        Type: ClusterType
        SpecificPatterns: (string * float32) list
        Amount: Time
        Rating: float32

        HoldCoverage: Percentiles<float32>
        Variety: Percentiles<float32>
        Density: Percentiles<Density>
    }
    static member OfCluster (cluster: Cluster<float32>) : PatternCluster =
        {
            Pattern = cluster.Pattern
            Type = cluster.Type
            SpecificPatterns = cluster.SpecificTypes
            Amount = cluster.Amount
            Rating = cluster.Rating
            HoldCoverage = cluster.HoldCoverage
            Variety = cluster.Variety
            Density = cluster.Density
        }

type CategoryInfo =
    {
        Amount: Time
        Difficulty: float32
        Importance : float32

        SpecificBPMs: (int<beat / minute / rate> * Time * float32) array
    }

type UncategorisedInfo =
    {
        Amount: Time
        Difficulty: float32
        Importance : float32
    }

[<RequireQualifiedAccess>]
type ChartTag =
    | Jacks
    | Chordstream
    | Stream
    | LN
    | SV
    | Beginner
    | Pure
    | Hybrid

/// Calculated dynamically for a specific chart + rate
/// Can have more details compared to the LibraryPatternInfo which is precalculated and stored for every chart
type PatternInfo =
    {
        Difficulty: float32
        Duration: Time

        SVAmount: Time
        HoldNotePercent: float32

        MainPatterns: PatternCluster array
        Purity: float32

        Jacks: CategoryInfo
        Chordstream: CategoryInfo
        Stream: CategoryInfo
        Uncategorised: UncategorisedInfo

        Tags: Set<ChartTag>

        Primitives: RowInfo<float32> list
        Segments: Segment<float32> array
    }

module PatternInfo =

    let [<Literal>] RELATIVE_IMPORTANCE_THRESHOLD = 0.2f
    let [<Literal>] PURITY_THRESHOLD = 0.7f
    let [<Literal>] MAJORITY_THRESHOLD = 0.5f

    let importance (duration: Time) (rating: float32) =
        duration / 1000.0f<ms> * rating * rating * rating

    let category_info (segments: Segment<float32> array) : CategoryInfo =

        let duration = segments |> Seq.sumBy (fun s -> s.End - s.Start)
        let rating = segments |> Seq.map _.Contents |> Seq.concat |> Seq.map _.Strains |> Seq.concat |> Seq.filter (fun x -> x > 0.0f) |> Difficulty.weighted_overall_difficulty
        let overall_importance = importance duration rating

        let bpm_groups =
            segments
            |> Seq.groupBy (fun s -> s.Type.BPM)
            |> Seq.map (fun (bpm, segs) ->
                let duration = segs |> Seq.sumBy (fun s -> s.End - s.Start)
                let rating = segs |> Seq.map _.Contents |> Seq.concat |> Seq.map _.Strains |> Seq.concat |> Seq.filter (fun x -> x > 0.0f) |> Difficulty.weighted_overall_difficulty
                (bpm, duration, rating), importance duration rating
            )
            |> Seq.sortByDescending snd
            |> Seq.filter (fun ((_, _, _), imp) -> imp > overall_importance * RELATIVE_IMPORTANCE_THRESHOLD)
            |> Seq.map fst
            |> Seq.toArray

        {
            Amount = duration
            Difficulty = rating
            Importance = overall_importance
            SpecificBPMs = bpm_groups
        }

    let uncategorised_info (segments: Segment<float32> array) : UncategorisedInfo =

        let duration = segments |> Seq.sumBy (fun s -> s.End - s.Start)
        let rating = segments |> Seq.map _.Contents |> Seq.concat |> Seq.map _.Strains |> Seq.concat |> Seq.filter (fun x -> x > 0.0f) |> Difficulty.weighted_overall_difficulty
        let overall_importance = importance duration rating

        {
            Amount = duration
            Difficulty = rating
            Importance = overall_importance
        }

    let from_chart_uncached (rate: Rate, chart: Chart) : PatternInfo =
        let difficulty = Difficulty.calculate (rate, chart.Notes)
        let patterns, primitives = Patterns.find_rate (chart, rate)

        let clusters =
            Clustering.get_clusters_rate patterns

        let main_clusters =
            Clustering.most_important(75<_>, clusters)
            |> Seq.map PatternCluster.OfCluster
            |> Seq.toArray

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
                    | Some (_, amount, rating) when importance amount rating / total > PURITY_THRESHOLD -> yield ChartTag.Pure
                    | _ -> ()
                elif amt_chordstream > MAJORITY_THRESHOLD then
                    yield ChartTag.Chordstream

                    match Array.tryHead chordstream.SpecificBPMs with
                    | Some (_, amount, rating) when importance amount rating / total > PURITY_THRESHOLD -> yield ChartTag.Pure
                    | _ -> ()
                elif amt_stream > MAJORITY_THRESHOLD then
                    yield ChartTag.Stream

                    match Array.tryHead stream.SpecificBPMs with
                    | Some (_, amount, rating) when importance amount rating / total > PURITY_THRESHOLD -> yield ChartTag.Pure
                    | _ -> ()

                elif amt_other > MAJORITY_THRESHOLD then
                    if uncategorised.Difficulty < 5.0f then yield ChartTag.Beginner else yield ChartTag.Hybrid

                elif amt_stream + amt_other > MAJORITY_THRESHOLD && amt_jack + amt_other < MAJORITY_THRESHOLD then
                    yield ChartTag.Stream

                elif amt_chordstream > amt_stream && amt_chordstream + amt_stream > MAJORITY_THRESHOLD && amt_jack + amt_other < MAJORITY_THRESHOLD then
                    yield ChartTag.Chordstream

                else
                    yield ChartTag.Hybrid

                if sv_time > 2000.0f<ms> then
                    yield ChartTag.SV
                if ln_percent > 0.25f then
                    yield ChartTag.LN
            }
            |> Set.ofSeq

        {
            Difficulty = difficulty.Overall
            Duration = chart.LastNote - chart.FirstNote

            SVAmount = sv_time
            HoldNotePercent = ln_percent

            MainPatterns = main_clusters
            Purity = 0.0f

            Jacks = jacks
            Chordstream = chordstream
            Stream = stream
            Uncategorised = uncategorised

            Tags = tags

            Primitives = primitives
            Segments = segments
        }

    let from_chart = from_chart_uncached |> cached