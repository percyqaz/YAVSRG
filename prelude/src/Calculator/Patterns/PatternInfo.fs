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

    member this.Format (rate: Rate) =

        let name =
            match this.SpecificPatterns with
            | (t, amount) :: _ when amount > 0.4f -> t
            | _ -> this.Pattern.ToString()

        match this.Type with
        | ClusterType.Normal bpm ->
            sprintf "%.0fBPM %s" (float32 bpm * rate) name
        | ClusterType.Mixed bpm ->
            sprintf "~%.0fBPM Mixed %s" (float32 bpm * rate) name
        | ClusterType.Combined -> name

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
    }

module PatternInfo =

    let from_chart_uncached (rate: Rate, chart: Chart) : PatternInfo =
        let difficulty = Difficulty.calculate (rate, chart.Notes)
        let patterns = Patterns.find_rate (chart, rate)

        let clusters =
            Clustering.get_clusters_rate patterns
            |> Array.filter (fun c -> not c.ShouldIgnore)

        let three_most_important =
            clusters
            |> Seq.sortByDescending (fun c -> c.Importance)
            |> Seq.truncate 3

        let three_hardest =
            clusters
            |> Seq.sortByDescending (fun c -> c.Rating)
            |> Seq.truncate 3

        let main_clusters =
            Seq.concat [three_most_important; three_hardest]
            |> Seq.distinct
            |> Seq.map PatternCluster.OfCluster
            |> Seq.toArray

        let sv_amount = Metrics.sv_time chart

        {
            Difficulty = difficulty.Overall
            Duration = chart.LastNote - chart.FirstNote

            SVAmount = sv_amount

            MainPatterns = main_clusters
            HoldNotePercent = Metrics.ln_percent chart
            Purity = 0.0f
            Simplicity = 0.0f
        }

    let from_chart = from_chart_uncached |> cached