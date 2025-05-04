namespace Prelude.Calculator.Patterns

open Prelude
open Prelude.Charts

type FoundPattern<'D> =
    {
        Pattern: CorePattern
        SpecificType: string option
        Mixed: bool
        Time: Time
        MsPerBeat: float32<ms/beat>
        Variety: float32
        Strains: 'D array
        HoldCoverage: float32
        Density: float32</rate>
    }

module internal Patterns =

    let private PATTERN_STABILITY_THRESHOLD = 5.0f<ms/beat>

    let private matches (specific_patterns: SpecificPatterns<'D>) (last_note: Time, primitives: RowInfo<'D> list) : FoundPattern<'D> array =
        let mutable remaining_data = primitives

        let results = ResizeArray()

        while not remaining_data.IsEmpty do
            match Core.STREAM remaining_data with
            | 0 -> ()
            | n ->
                let n, specific_type =
                    specific_patterns.Stream
                    |> List.tryPick (fun (name, p) -> p remaining_data |> function 0 -> None | n -> Some (n, name))
                    |> function None -> (n, None) | Some (m, specific_type) -> max n m, Some specific_type

                let d = List.take n remaining_data
                let mean_mspb = d |> List.averageBy _.MsPerBeat

                results.Add {
                    Pattern = Stream
                    SpecificType = specific_type
                    Mixed = d |> List.forall (fun d -> abs(d.MsPerBeat - mean_mspb) < PATTERN_STABILITY_THRESHOLD) |> not
                    Time = remaining_data.Head.Time
                    MsPerBeat = mean_mspb
                    Variety = d |> List.averageBy _.Variety
                    Strains = remaining_data.Head.Strains
                    HoldCoverage = d |> List.averageBy _.HoldCoverage
                    Density = d |> List.averageBy _.Density
                }

            match Core.CHORDSTREAM remaining_data with
            | 0 -> ()
            | n ->
                let n, specific_type =
                    specific_patterns.Chordstream
                    |> List.tryPick (fun (name, p) -> p remaining_data |> function 0 -> None | n -> Some (n, name))
                    |> function None -> (n, None) | Some (m, specific_type) -> max n m, Some specific_type

                let d = List.take n remaining_data
                let mean_mspb = d |> List.averageBy _.MsPerBeat

                results.Add {
                    Pattern = Chordstream
                    SpecificType = specific_type
                    Mixed = d |> List.forall (fun d -> abs(d.MsPerBeat - mean_mspb) < PATTERN_STABILITY_THRESHOLD) |> not
                    Time = remaining_data.Head.Time
                    MsPerBeat = mean_mspb
                    Variety = d |> List.averageBy _.Variety
                    Strains = remaining_data.Head.Strains
                    HoldCoverage = d |> List.averageBy _.HoldCoverage
                    Density = d |> List.averageBy _.Density
                }

            match Core.JACKS remaining_data with
            | 0 -> ()
            | n ->
                let n, specific_type =
                    specific_patterns.Jack
                    |> List.tryPick (fun (name, p) -> p remaining_data |> function 0 -> None | n -> Some (n, name))
                    |> function None -> (n, None) | Some (m, specific_type) -> max n m, Some specific_type

                let d = List.take n remaining_data
                let mean_mspb = d |> List.averageBy _.MsPerBeat

                results.Add {
                    Pattern = Jacks
                    SpecificType = specific_type
                    Mixed = d |> List.forall (fun d -> abs(d.MsPerBeat - mean_mspb) < PATTERN_STABILITY_THRESHOLD) |> not
                    Time = remaining_data.Head.Time
                    MsPerBeat = mean_mspb
                    Variety = d |> List.averageBy _.Variety
                    Strains = remaining_data.Head.Strains
                    HoldCoverage = d |> List.averageBy _.HoldCoverage
                    Density = d |> List.averageBy _.Density
                }

            remaining_data <- List.tail remaining_data

        results.ToArray()

    let keymode_patterns<'D> (keymode: int) : SpecificPatterns<'D> =
        if keymode = 4 then
            SpecificPatterns.SPECIFIC_4K
        elif keymode = 7 then
            SpecificPatterns.SPECIFIC_7K
        else
            SpecificPatterns.SPECIFIC_OTHER

    let find_rate (chart: Chart, rate: Rate) : FoundPattern<float32> array * RowInfo<float32> list =
        let primitives = Primitives.calculate_rate (chart, rate)
        matches (keymode_patterns chart.Keys) (chart.LastNote, primitives), primitives

    let find_multirate (chart: Chart) : FoundPattern<float32 * float32> array =
        let primitives = Primitives.calculate_multirate (chart)
        matches (keymode_patterns chart.Keys) (chart.LastNote, primitives)