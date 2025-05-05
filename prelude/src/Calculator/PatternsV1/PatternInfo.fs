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
        Simplicity: float32

        Primitives: RowInfo<float32> list
        Segments: Segment<float32> array
    }

module PatternInfo =

    let groups (segments: Segment<float32> array) : SegmentGroup<float32> array =
        segments
        |> Seq.groupBy (fun s -> s.Type)
        |> Seq.map (fun (t, segs) ->
            let total = segs |> Seq.sumBy (fun s -> s.End - s.Start)
            let rating = segs |> Seq.map _.Contents |> Seq.concat |> Seq.map _.Strains |> Seq.concat |> Seq.filter (fun x -> x > 0.0f) |> Difficulty.weighted_overall_difficulty
            { Type = t; Total = total; Rating = rating })
        |> Seq.sortByDescending _.Importance
        |> Seq.toArray

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
        let groups = groups segments

        {
            Difficulty = difficulty.Overall
            Duration = chart.LastNote - chart.FirstNote

            SVAmount = Metrics.sv_time chart
            HoldNotePercent = Metrics.ln_percent chart

            MainPatterns = main_clusters
            Purity = 0.0f
            Simplicity = 0.0f

            Primitives = primitives
            Segments = segments
        }

    let from_chart = from_chart_uncached |> cached