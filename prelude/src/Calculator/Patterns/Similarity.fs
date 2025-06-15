namespace Prelude.Calculator.Patterns

open Prelude.Calculator.Patterns

module Similarity =

    let calculate (chart_a: LibraryPatternInfo) (chart_b: LibraryPatternInfo) =
        let a_total =
            chart_a.Jacks.Importance + chart_a.Chordstream.Importance + chart_a.Stream.Importance + chart_a.Uncategorised.Importance

        let b_total =
            chart_b.Jacks.Importance + chart_b.Chordstream.Importance + chart_b.Stream.Importance + chart_b.Uncategorised.Importance

        let jack_difference =
            abs (chart_a.Jacks.Importance / a_total - chart_b.Jacks.Importance / b_total)

        let chordstream_difference =
            abs (chart_a.Chordstream.Importance / a_total - chart_b.Chordstream.Importance / b_total)

        let stream_difference =
            abs (chart_a.Stream.Importance / a_total - chart_b.Stream.Importance / b_total)

        let uncat_difference =
            abs (chart_a.Uncategorised.Importance / a_total - chart_b.Uncategorised.Importance / b_total)

        let difference =
            (
                jack_difference * jack_difference
                + chordstream_difference * chordstream_difference
                + stream_difference * stream_difference
                + uncat_difference * uncat_difference
            )
            |> sqrt

        1.0f - difference / 1.4142136f