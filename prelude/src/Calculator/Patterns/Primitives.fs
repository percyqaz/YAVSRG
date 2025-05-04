namespace Prelude.Calculator.Patterns

open Percyqaz.Common
open Prelude
open Prelude.Charts
open Prelude.Calculator

type BPMCluster =
    internal {
        mutable SumMs: float32<ms / beat>
        OriginalMsPerBeat: float32<ms / beat>
        mutable Count: int
        mutable BPM: int<beat / minute / rate> option
    }
    member internal this.Add value =
        this.Count <- this.Count + 1
        this.SumMs <- this.SumMs + value

    member internal this.Calculate() =
        let average = this.SumMs / float32 this.Count
        this.BPM <- 60000.0f<ms / minute> / average |> float32 |> round |> int |> fun x -> x * 1<beat / minute / rate> |> Some

    member this.Value = this.BPM.Value

[<RequireQualifiedAccess>]
type Direction =
    | None
    | Left
    | Right
    | Outwards
    | Inwards

type RowInfo<'D> =
    {
        Index: int
        Time: Time
        MsPerBeat: float32<ms / beat>
        BPM: BPMCluster
        Notes: int
        Jacks: int
        Direction: Direction
        Roll: bool
        Density: Density
        Variety: float32 // todo: consider 'D for this too
        HoldCoverage: float32
        Strains: 'D array
        RawNotes: int array
    }

module internal Primitives =

    let detect_direction (previous_row: int array) (current_row: int array) : Direction * bool =
        assert (previous_row.Length > 0)
        assert (current_row.Length > 0)

        let pleftmost = Array.head previous_row
        let prightmost = Array.last previous_row
        let cleftmost = Array.head current_row
        let crightmost = Array.last current_row

        let leftmost_change = cleftmost - pleftmost
        let rightmost_change = crightmost - prightmost

        let direction =
            if leftmost_change > 0 then
                if rightmost_change > 0 then
                    Direction.Right
                else
                    Direction.Inwards
            elif leftmost_change < 0 then
                if rightmost_change < 0 then
                    Direction.Left
                else
                    Direction.Outwards
            else
                if rightmost_change < 0 then
                    Direction.Inwards
                elif rightmost_change > 0 then
                    Direction.Outwards
                else
                    Direction.None
        let is_roll = pleftmost > crightmost || prightmost < cleftmost
        direction, is_roll

    [<Literal>]
    let private BPM_CLUSTER_THRESHOLD = 5.0f<ms / beat>

    let private assign_cluster (bpms: ResizeArray<BPMCluster>, ms_per_beat: float32<ms / beat>) : BPMCluster =
        match
            bpms |> Seq.tryFind (fun c -> abs (c.OriginalMsPerBeat - ms_per_beat) < BPM_CLUSTER_THRESHOLD)
        with
        | Some existing_cluster ->
            existing_cluster.Add ms_per_beat
            existing_cluster
        | None ->
            let new_cluster =
                {
                    Count = 1
                    SumMs = ms_per_beat
                    OriginalMsPerBeat = ms_per_beat
                    BPM = None
                }
            bpms.Add new_cluster
            new_cluster

    let private calculate (density: Density array, hold_coverage: float32 array, get_variety: int -> float32, get_strains: int -> 'D array, chart: Chart) : RowInfo<'D> list =

        let bpms = ResizeArray<BPMCluster>()
        let { Time = first_note; Data = row } = (TimeArray.first chart.Notes).Value

        let mutable previous_row =
            seq { 0 .. chart.Keys - 1 }
            |> Seq.filter (fun k -> row.[k] = NoteType.NORMAL || row.[k] = NoteType.HOLDHEAD)
            |> Array.ofSeq

        if previous_row.Length = 0 then
            Logging.Error("First row of chart is empty, wtf?")
            []
        else

        let mutable previous_time = first_note
        let mutable index = 0

        let results =
            seq {
                for { Time = t; Data = row } in (chart.Notes |> Seq.skip 1) do
                    index <- index + 1

                    let current_row =
                        seq { 0 .. chart.Keys - 1 }
                        |> Seq.filter (fun k -> row.[k] = NoteType.NORMAL || row.[k] = NoteType.HOLDHEAD)
                        |> Array.ofSeq

                    if current_row.Length > 0 then

                        let direction, is_roll = detect_direction previous_row current_row
                        let mspb = (t - previous_time) * 4.0f< / beat>

                        yield
                            {
                                Index = index
                                Time = (t - first_note)
                                MsPerBeat = mspb
                                BPM = assign_cluster(bpms, mspb)
                                Notes = current_row.Length
                                Jacks = current_row.Length - (Array.except previous_row current_row).Length
                                Direction = direction
                                Roll = is_roll
                                Density = density.[index]
                                HoldCoverage = hold_coverage.[index]
                                Variety = get_variety index
                                Strains = get_strains index
                                RawNotes = current_row
                            }

                        previous_row <- current_row
                        previous_time <- t
            }
            |> List.ofSeq
        bpms |> Seq.iter (fun c -> c.Calculate())
        results

    let calculate_rate (chart: Chart, rate: Rate) : RowInfo<float32> list =

        let density = Density.process_chart chart
        let hold_coverage = HoldCoverage.calculate_coverage (chart.Keys, chart.Notes, rate)
        let difficulty_info = Difficulty.calculate (rate, chart.Notes)

        calculate(
            density,
            hold_coverage,
            (fun i -> difficulty_info.Variety.[i]),
            (fun i -> difficulty_info.Strains.[i].StrainV1Notes),
            chart
        )

    let calculate_multirate (chart: Chart) : RowInfo<float32 * float32> list =
        let density = Density.process_chart chart
        let hold_coverage = HoldCoverage.calculate_coverage (chart.Keys, chart.Notes, 1.0f<rate>)
        let difficulty100 = Difficulty.calculate (1.0f<rate>, chart.Notes)
        let difficulty150 = Difficulty.calculate (1.0f<rate>, chart.Notes)

        calculate(
            density,
            hold_coverage,
            (fun i -> difficulty100.Variety.[i]), // todo: consider using both
            (fun i -> Array.zip difficulty100.Strains.[i].StrainV1Notes difficulty150.Strains.[i].StrainV1Notes),
            chart
        )

module Metrics =

    let ln_percent (chart: Chart) : float32 =
        let mutable notes = 0
        let mutable lnotes = 0

        for { Data = nr } in chart.Notes do
            for n in nr do
                if n = NoteType.NORMAL then
                    notes <- notes + 1
                elif n = NoteType.HOLDHEAD then
                    notes <- notes + 1
                    lnotes <- lnotes + 1

        float32 lnotes / float32 notes

    let sv_time (chart: Chart) : Time =
        if chart.SV.Length = 0 then
            0.0f<ms>
        else

            let mutable total = 0.0f<ms>

            let mutable time = chart.FirstNote
            let mutable vel = 1.0f

            for sv in chart.SV do
                if not (System.Single.IsFinite vel) || abs (vel - 1.0f) > 0.01f then
                    total <- total + (sv.Time - time)

                vel <- sv.Data
                time <- sv.Time

            if not (System.Single.IsFinite vel) || abs (vel - 1.0f) > 0.01f then
                total <- total + (chart.LastNote - time)

            total