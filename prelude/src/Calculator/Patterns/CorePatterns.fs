namespace Prelude.Calculator.Patterns

open Prelude

[<RequireQualifiedAccess>]
type SegmentType =
    | Stream of bpm: int<beat / minute / rate>
    | Chordstream of bpm: int<beat / minute / rate> * chord_density: float32
    | Jacks of bpm: int<beat / minute / rate> * chord_density: float32
    | Uncategorized

type Segment<'D> =
    {
        Type: SegmentType
        Start: Time
        End: Time
        Contents: RowInfo<'D> list
    }

module CorePatternParser =

    [<RequireQualifiedAccess>]
    type CorePatternType =
    | Stream
    | Chordstream
    | Jacks
    | Uncategorized

    [<Literal>]
    let MIN_JACK_BPM = 60<beat / minute / rate>
    [<Literal>]
    let JACK_FORGIVENESS_COUNT = 2

    let internal detect_jacks (rows: RowInfo<'D> list) : RowInfo<'D> list * RowInfo<'D> list =

        match rows with
        | [] -> [], []
        | r :: rs when r.Jacks > 0 && r.BPM.Value > MIN_JACK_BPM ->
            let bpm = r.BPM.Value

            let rec loop (forgive_non_jack: int) (matched: RowInfo<'D> list) (remaining: RowInfo<'D> list) : RowInfo<'D> list * RowInfo<'D> list =
                match remaining with
                | r :: rs when r.Jacks > 0 && r.BPM.Value = bpm ->
                    loop JACK_FORGIVENESS_COUNT (r :: matched) rs
                | r :: rs when forgive_non_jack > 0 ->
                    loop (forgive_non_jack - 1) (r :: matched) rs
                | remaining -> matched, remaining

            loop JACK_FORGIVENESS_COUNT [r] rs

        | r :: rs -> [], r :: rs

    let parse (rows: RowInfo<'D> list) : (RowInfo<'D> list * CorePatternType) seq =

        let mutable uncategorized = []
        seq {
            let mutable remaining = rows
            while remaining.Length > 0 do
                let matched, rest = detect_jacks remaining
                remaining <- rest
                if matched.Length > 0 then
                    if uncategorized.Length > 0 then
                        yield uncategorized, CorePatternType.Uncategorized
                        uncategorized <- []
                    yield matched, CorePatternType.Jacks
                else
                    uncategorized <- List.head remaining :: uncategorized
                    remaining <- List.tail remaining

            if uncategorized.Length > 0 then
                yield uncategorized, CorePatternType.Uncategorized
        }

    let make_segment (rows: RowInfo<'D> list, core_type: CorePatternType) : Segment<'D> =
        let bpm = rows |> Seq.last |> fun r -> r.BPM.Value
        let t_start = rows |> Seq.last |> fun r -> r.Time
        let t_end = rows |> Seq.head |> fun r -> r.Time

        match core_type with
        | CorePatternType.Stream ->
            {
                Type = SegmentType.Stream bpm
                Start = t_start
                End = t_end
                Contents = rows
            }
        | CorePatternType.Chordstream ->
            let chord_density = rows |> Seq.averageBy (fun r -> float32 r.Notes * 2.0f)
            {
                Type = SegmentType.Chordstream(bpm, chord_density)
                Start = t_start
                End = t_end
                Contents = rows
            }
        | CorePatternType.Jacks ->
            let chord_density = rows |> Seq.averageBy (fun r -> float32 r.Notes)
            {
                Type = SegmentType.Jacks(bpm, chord_density)
                Start = t_start
                End = t_end
                Contents = rows
            }
        | CorePatternType.Uncategorized ->
            {
                Type = SegmentType.Uncategorized
                Start = t_start
                End = t_end
                Contents = rows
            }