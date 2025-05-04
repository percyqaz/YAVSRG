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

    let from_chart_uncached (rate: Rate, chart: Chart) : PatternInfo =
        let difficulty = Difficulty.calculate (rate, chart.Notes)
        let patterns, primitives = Patterns.find_rate (chart, rate)

        let clusters =
            Clustering.get_clusters_rate patterns

        let pattern_amount (pattern_type: CorePattern) =
            clusters
            |> Seq.tryFind (fun c -> c.Pattern = pattern_type && c.Type.IsCombined)
            |> Option.map _.Amount
            |> Option.defaultValue 0.0f<ms>

        let duration = chart.LastNote - chart.FirstNote
        let jacks = pattern_amount Jacks
        let chordstream = pattern_amount Chordstream
        let stream = pattern_amount Stream
        let purity =
            let total = jacks + chordstream + stream |> max duration
            let p = Seq.max [| jacks / total; chordstream / total; stream / total |]
            (p - (1.0f / 3.0f)) * 1.5f |> min 1.0f |> max 0.0f

        let main_clusters =
            Clustering.most_important(75<_>, clusters)
            |> Seq.map PatternCluster.OfCluster
            |> Seq.toArray

        let complexity =
            if main_clusters.Length = 0 then 0.0f
            else
                let v = main_clusters |> Seq.averageBy (_.Variety.P50)
                (v - 5.0f) / 15.0f |> min 1.0f |> max 0.0f

        let sv_amount = Metrics.sv_time chart

        {
            Difficulty = difficulty.Overall
            Duration = chart.LastNote - chart.FirstNote

            SVAmount = sv_amount

            MainPatterns = main_clusters
            HoldNotePercent = Metrics.ln_percent chart
            Purity = purity
            Simplicity = 1.0f - complexity

            Primitives = primitives
            Segments = CorePatternParser.parse primitives |> Seq.map CorePatternParser.make_segment |> Array.ofSeq
        }

    let from_chart = from_chart_uncached |> cached