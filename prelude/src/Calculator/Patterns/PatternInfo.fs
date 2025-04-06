namespace Prelude.Calculator.Patterns

open Prelude
open Prelude.Charts
open Prelude.Calculator

// todo: wonder about making LN sections separate to the main pattern
// 7k files like to have a big LN spam at the end that should register separately
type PatternCluster =
    {
        Pattern: CorePattern
        BPM: int<beat / minute>
        Mixed: bool
        Amount: Time
        Difficulty: float32

        HoldCoverage: Percentiles<float32>
        Variety: Percentiles<float32>
        Density: Percentiles<Density>
    }

/// Calculated dynamically for a specific chart + rate
/// Can have more details compared to the LibraryPatternInfo which is precalculated and stored for every chart
type PatternInfo =
    {
        Difficulty: float32
        SVAmount: Time
        Duration: Time

        MainPatterns: PatternCluster array
        HoldNotePercent: float32
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
            |> Seq.take 3

        let three_hardest =
            clusters
            |> Seq.sortByDescending (fun c -> c.Rating)
            |> Seq.take 3

        let main_clusters = Seq.concat [three_most_important; three_hardest] |> Seq.distinct

        let sv_amount = Metrics.sv_time chart

        {
            Difficulty = difficulty.Overall
            SVAmount = sv_amount
            Duration = chart.LastNote - chart.FirstNote

            MainPatterns = [||]
            HoldNotePercent = Metrics.ln_percent chart
            Purity = 0.0f
            Simplicity = 0.0f
        }

    let from_chart = from_chart_uncached |> cached