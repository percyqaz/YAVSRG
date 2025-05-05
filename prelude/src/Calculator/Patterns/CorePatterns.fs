namespace Prelude.Calculator.Patterns

open Prelude

[<RequireQualifiedAccess>]
type SegmentType =
    | Stream of bpm: int<beat / minute / rate>
    | Chordstream of bpm: int<beat / minute / rate>
    | Jacks of bpm: int<beat / minute / rate>
    | Uncategorized

type Segment<'D> =
    {
        Type: SegmentType
        Start: Time
        End: Time
        Contents: RowInfo<'D> list
    }

type SegmentGroup<'D> =
    {
        Type: SegmentType
        Total: Time
        Rating: float32
    }
    member this.Importance = this.Total / 1000.0f<ms> * this.Rating * this.Rating

module CorePatternParser =

    [<RequireQualifiedAccess>]
    type CorePatternType =
    | Stream
    | Chordstream
    | Jacks
    | Uncategorized

    let [<Literal>] MIN_JACK_BPM = 60<beat / minute / rate>
    let [<Literal>] JACK_FORGIVENESS_COUNT = 2
    let [<Literal>] MIN_STREAM_BPM = 120<beat / minute / rate>
    /// Number of notes in 4 chords that counts as chordstream over stream
    /// e.g. 2-1-1-1 adds up to 5 and is stream, 3-1-1-1 and 2-1-1-2 are chordstream
    let [<Literal>] CHORDSTREAM_THRESHOLD = 6

    let internal detect_jacks (rows: RowInfo<'D> list) : RowInfo<'D> list * RowInfo<'D> list =

        match rows with
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

        | otherwise -> [], otherwise

    let internal detect_chordstream (rows: RowInfo<'D> list) : RowInfo<'D> list * RowInfo<'D> list =
        match rows with
        | a :: b :: c :: d :: rs
            when
                a.BPM.Value > MIN_STREAM_BPM
                && b.Jacks = 0 && c.Jacks = 0 && d.Jacks = 0
                && b.BPM.Value = a.BPM.Value && c.BPM.Value = a.BPM.Value && d.BPM.Value = a.BPM.Value
                && a.Notes + b.Notes + c.Notes + d.Notes >= CHORDSTREAM_THRESHOLD
            ->
            let bpm = a.BPM.Value
            let rec loop (n1: int, n2: int, n3: int) (matched: RowInfo<'D> list) (remaining: RowInfo<'D> list) : RowInfo<'D> list * RowInfo<'D> list =
                match remaining with
                | r :: rs when
                    r.Notes > 0
                    && r.Jacks = 0
                    && r.BPM.Value = bpm
                    && r.Notes + n1 + n2 + n3 >= CHORDSTREAM_THRESHOLD
                    ->
                    loop (r.Notes, n1, n2) (r :: matched) rs
                | remaining -> matched, remaining
            loop (d.Notes, c.Notes, b.Notes) [d; c; b; a] rs

        | otherwise -> [], otherwise

    let internal detect_stream (rows: RowInfo<'D> list) : RowInfo<'D> list * RowInfo<'D> list =
        match rows with
        | a :: b :: c :: d :: rs
            when
                a.BPM.Value > MIN_STREAM_BPM
                && b.Jacks = 0 && c.Jacks = 0 && d.Jacks = 0
                && b.BPM.Value = a.BPM.Value && c.BPM.Value = a.BPM.Value && d.BPM.Value = a.BPM.Value
                && a.Notes + b.Notes + c.Notes + d.Notes < CHORDSTREAM_THRESHOLD
            ->
            let bpm = a.BPM.Value
            let rec loop (n1: int, n2: int, n3: int) (matched: RowInfo<'D> list) (remaining: RowInfo<'D> list) : RowInfo<'D> list * RowInfo<'D> list =
                match remaining with
                | r :: rs when
                    r.Notes > 0
                    && r.Jacks = 0
                    && r.BPM.Value = bpm
                    && r.Notes + n1 + n2 + n3 < CHORDSTREAM_THRESHOLD
                    ->
                    loop (r.Notes, n1, n2) (r :: matched) rs
                | remaining -> matched, remaining
            loop (d.Notes, c.Notes, b.Notes) [d; c; b; a] rs

        | otherwise -> [], otherwise

    let parse (rows: RowInfo<'D> list) : (RowInfo<'D> list * CorePatternType) seq =

        let mutable uncategorized = []
        seq {
            let mutable remaining = rows
            while not (List.isEmpty remaining) do
                let j_matched, rest = detect_jacks remaining
                remaining <- rest
                if not (List.isEmpty j_matched) then
                    if not (List.isEmpty uncategorized) then
                        yield uncategorized, CorePatternType.Uncategorized
                        uncategorized <- []
                    yield j_matched, CorePatternType.Jacks
                else

                let cs_matched, rest = detect_chordstream remaining
                remaining <- rest

                if not (List.isEmpty cs_matched) then
                    if not (List.isEmpty uncategorized) then
                        yield uncategorized, CorePatternType.Uncategorized
                        uncategorized <- []
                    yield cs_matched, CorePatternType.Chordstream
                else

                let s_matched, rest = detect_stream remaining
                remaining <- rest

                if not (List.isEmpty s_matched) then
                    if not (List.isEmpty uncategorized) then
                        yield uncategorized, CorePatternType.Uncategorized
                        uncategorized <- []
                    yield s_matched, CorePatternType.Stream

                else
                    uncategorized <- List.head remaining :: uncategorized
                    remaining <- List.tail remaining

            if not (List.isEmpty uncategorized) then
                yield uncategorized, CorePatternType.Uncategorized
        }

    let make_segment (rows: RowInfo<'D> list, core_type: CorePatternType) : Segment<'D> =
        let last = rows |> Seq.last
        let bpm = last.BPM.Value
        let t_start = last.Time - last.MsPerBeat / 4.0f</beat>
        let t_end = rows |> Seq.head |> fun r -> r.Time

        match core_type with
        | CorePatternType.Stream ->
            {
                Type = SegmentType.Stream(bpm)
                Start = t_start
                End = t_end
                Contents = rows
            }
        | CorePatternType.Chordstream ->
            let chord_density = rows |> Seq.averageBy (fun r -> float32 r.Notes * 2.0f)
            {
                Type = SegmentType.Chordstream(bpm)
                Start = t_start
                End = t_end
                Contents = rows
            }
        | CorePatternType.Jacks ->
            let chord_density = rows |> Seq.averageBy (fun r -> float32 r.Notes)
            {
                Type = SegmentType.Jacks(bpm)
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