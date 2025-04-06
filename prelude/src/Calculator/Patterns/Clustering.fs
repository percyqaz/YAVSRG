namespace Prelude.Calculator.Patterns

open System.Collections.Generic
open Percyqaz.Data
open Prelude
open Prelude.Calculator

type private ClusterBuilder =
    internal {
        mutable SumMs: float32<ms / beat>
        OriginalMsPerBeat: float32<ms / beat>
        mutable Count: int
        mutable BPM: int<beat / minute / rate> option
    }
    member this.Add value =
        this.Count <- this.Count + 1
        this.SumMs <- this.SumMs + value

    member this.Calculate() =
        let average = this.SumMs / float32 this.Count
        this.BPM <- 60000.0f<ms / minute> / average |> float32 |> round |> int |> fun x -> x * 1<beat / minute / rate> |> Some

    member this.Value = this.BPM.Value

[<Struct>]
[<Json.AutoCodec>]
type Percentiles<'T> =
    {
        P10: 'T
        P25: 'T
        P50: 'T
        P75: 'T
        P90: 'T
    }

module Percentiles =

    let zero (value: 'T) =
        {
            P10 = value
            P25 = value
            P50 = value
            P75 = value
            P90 = value
        }

    let find (percentile: float32, sorted_values: float32<'u> array) : float32<'u> =
        if sorted_values.Length = 0 then 0.0f |> LanguagePrimitives.Float32WithMeasure else
        let index = percentile * float32 sorted_values.Length |> floor |> int
        sorted_values.[index]

    let create (sorted_values: float32<'u> array) : Percentiles<float32<'u>> =
        {
            P10 = find(0.1f, sorted_values)
            P25 = find(0.25f, sorted_values)
            P50 = find(0.5f, sorted_values)
            P75 = find(0.75f, sorted_values)
            P90 = find(0.9f, sorted_values)
        }

[<RequireQualifiedAccess>]
[<Json.AutoCodec>]
type ClusterType =
    | Normal of bpm: int<beat / minute / rate>
    | Mixed of bpm: int<beat / minute / rate>
    | Combined of min_bpm: int<beat / minute / rate> * max_bpm: int<beat / minute / rate>

[<Json.AutoCodec>]
type Cluster<'D> =
    {
        Pattern: CorePattern
        Type: ClusterType
        SpecificTypes: (string * float32) list // todo: rename SpecificPatterns

        Rating: 'D

        HoldCoverage: Percentiles<float32>
        Variety: Percentiles<float32>
        Density: Percentiles<Density>

        Amount: Time
    }

    member this.BPM : int<beat / minute / rate> =
        match this.Type with
        | ClusterType.Normal bpm -> bpm
        | ClusterType.Mixed bpm -> bpm
        | ClusterType.Combined (_, bpm) -> bpm

    static member Default =
        {
            Pattern = Jacks
            SpecificTypes = []
            Type = ClusterType.Normal 120<beat / minute / rate>

            Rating = 0.0f

            HoldCoverage = Percentiles.zero(0.0f)
            Variety = Percentiles.zero(0.0f)
            Density = Percentiles.zero(0.0f</rate>)

            Amount = 1.0f<ms>
        }

    member this.Importance : float32 =
        this.Amount
        * this.Pattern.DensityToBPM
        * this.Density.P50
        * (match this.Type with ClusterType.Combined _ -> 0.3f | _ -> 1.0f)
        |> float32

    member this.Supersedes (other: Cluster<'D>) : bool =
        if this.Pattern = other.Pattern then

            this.Type.IsCombined
            || other.Type.IsCombined
            || (this.Amount * 0.5f > other.Amount && this.BPM > other.BPM)

        else false

module private Clustering =

    let BPM_CLUSTER_THRESHOLD = 5.0f<ms / beat>

    let private pattern_amount (sorted_starts_ends: (Time * Time) array) : Time =

        let mutable total_time: Time = 0.0f<ms>

        let a, b = Array.head sorted_starts_ends

        let mutable current_start = a
        let mutable current_end = b

        for start, _end in sorted_starts_ends do
            if current_end < _end then
                total_time <- total_time + (current_end - current_start)

                current_start <- start
                current_end <- _end
            else
                current_end <- max current_end _end

        total_time <- total_time +  (current_end - current_start)

        total_time

    let assign_clusters (patterns: FoundPattern<'D> array) : (FoundPattern<'D> * ClusterBuilder) array =
        let bpms_non_mixed = ResizeArray<ClusterBuilder>()
        let bpms_mixed = Dictionary<CorePattern, ClusterBuilder>()

        let add_to_cluster ms_per_beat : ClusterBuilder =
            match
                bpms_non_mixed
                |> Seq.tryFind (fun c -> abs (c.OriginalMsPerBeat - ms_per_beat) < BPM_CLUSTER_THRESHOLD)
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

                bpms_non_mixed.Add new_cluster
                new_cluster

        let add_to_mixed_cluster pattern value : ClusterBuilder =
            if bpms_mixed.ContainsKey pattern then
                let existing_cluster = bpms_mixed.[pattern]
                existing_cluster.Add value
                existing_cluster
            else
                let new_cluster =
                    {
                        Count = 1
                        SumMs = value
                        OriginalMsPerBeat = value
                        BPM = None
                    }

                bpms_mixed.Add(pattern, new_cluster)
                new_cluster

        let patterns_with_clusters =
            patterns
            |> Array.map (fun pattern ->
                pattern,
                if pattern.Mixed then
                    add_to_mixed_cluster pattern.Pattern pattern.MsPerBeat
                else
                    add_to_cluster pattern.MsPerBeat
            )

        bpms_non_mixed |> Seq.iter (fun cluster -> cluster.Calculate())
        bpms_mixed.Values |> Seq.iter (fun cluster -> cluster.Calculate())

        patterns_with_clusters

    let specific_clusters (rate_difficulty: 'D seq -> 'D, patterns_with_clusters: (FoundPattern<'D> * ClusterBuilder) array) : Cluster<'D> array =

        patterns_with_clusters
        |> Array.groupBy (fun (pattern, c) ->
            pattern.Pattern, pattern.Mixed, c.Value
        )
        |> Array.map (fun ((pattern, mixed, bpm), data) ->
            let starts_ends = data |> Array.map (fun (m, _) -> m.Start, m.End)
            let densities = data |> Array.map (fst >> _.Density) |> Array.sort
            let varieties = data |> Array.map (fst >> _.Variety) |> Array.sort
            let hold_coverages = data |> Array.map (fst >> _.HoldCoverage) |> Array.sort

            let data_count = float32 data.Length
            let specific_types =
                data
                |> Array.choose (fst >> _.SpecificType)
                |> Array.countBy id
                |> Seq.map (fun (specific_type, count) -> (specific_type, float32 count / data_count))
                |> Seq.sortByDescending snd
                |> List.ofSeq

            {
                Pattern = pattern
                Type = if mixed then ClusterType.Mixed bpm else ClusterType.Normal bpm
                SpecificTypes = specific_types

                Rating = data |> Seq.map (fst >> _.Strains) |> Seq.concat |> rate_difficulty

                HoldCoverage = Percentiles.create hold_coverages
                Variety = Percentiles.create varieties
                Density = Percentiles.create densities

                Amount = pattern_amount starts_ends
            }
        )

    let core_pattern_cluster (pattern_type: CorePattern, rate_difficulty: 'D seq -> 'D, patterns_with_clusters: (FoundPattern<'D> * ClusterBuilder) array) : Cluster<'D> option =

        let data =
            patterns_with_clusters
            |> Array.filter (fun (pattern, _) -> pattern.Pattern = pattern_type)

        if data.Length = 0 then None else

        let starts_ends = data |> Array.map (fun (m, _) -> m.Start, m.End)
        let densities = data |> Array.map (fst >> _.Density) |> Array.sort
        let varieties = data |> Array.map (fst >> _.Variety) |> Array.sort
        let hold_coverages = data |> Array.map (fst >> _.HoldCoverage) |> Array.sort

        let bpms = data |> Seq.map (snd >> _.Value) |> Seq.distinct

        let data_count = float32 data.Length
        let specific_types =
            data
            |> Array.choose (fst >> _.SpecificType)
            |> Array.countBy id
            |> Seq.map (fun (specific_type, count) -> (specific_type, float32 count / data_count))
            |> Seq.sortByDescending snd
            |> List.ofSeq

        Some {
            Pattern = pattern_type
            SpecificTypes = specific_types
            Type = ClusterType.Combined (Seq.min bpms, Seq.max bpms)

            Rating = data |> Seq.map (fst >> _.Strains) |> Seq.concat |> rate_difficulty

            HoldCoverage = Percentiles.create hold_coverages
            Variety = Percentiles.create varieties
            Density = Percentiles.create densities

            Amount = pattern_amount starts_ends
        }

    let get_clusters_rate (patterns: FoundPattern<float32> array) : Cluster<float32> array =
        let patterns_with_clusters = assign_clusters patterns
        let difficulties_to_rating = Seq.filter (fun x -> x > 0.0f) >> Difficulty.weighted_overall_difficulty

        seq {
            yield! specific_clusters (difficulties_to_rating, patterns_with_clusters)
            yield! core_pattern_cluster (Jacks, difficulties_to_rating, patterns_with_clusters) |> Option.toList
            yield! core_pattern_cluster (Chordstream, difficulties_to_rating, patterns_with_clusters) |> Option.toList
            yield! core_pattern_cluster (Stream, difficulties_to_rating, patterns_with_clusters) |> Option.toList
        }
        |> Array.ofSeq

    let get_clusters_multirate (patterns: FoundPattern<float32 * float32> array) : Cluster<float32 * float32> array =
        let patterns_with_clusters = assign_clusters patterns

        let difficulties_to_ratings (ds: (float32 * float32) seq) =
            let values =
                ds
                |> Seq.filter (fun (x, _) -> x > 0.0f)
                |> Array.ofSeq
            Difficulty.weighted_overall_difficulty (Seq.map fst values),
            Difficulty.weighted_overall_difficulty (Seq.map snd values)

        seq {
            yield! specific_clusters (difficulties_to_ratings, patterns_with_clusters)
            yield! core_pattern_cluster (Jacks, difficulties_to_ratings, patterns_with_clusters) |> Option.toList
            yield! core_pattern_cluster (Chordstream, difficulties_to_ratings, patterns_with_clusters) |> Option.toList
            yield! core_pattern_cluster (Stream, difficulties_to_ratings, patterns_with_clusters) |> Option.toList
        }
        |> Array.ofSeq

    let most_important (min_bpm: int<beat / minute / rate>, clusters: Cluster<'D> array) : Cluster<'D> seq =
        let mutable remaining =
            clusters
            |> Seq.where (fun c -> c.BPM >= min_bpm && c.BPM <= 600<beat / minute / rate>)
            |> Seq.sortByDescending (fun c -> c.Importance)
            |> List.ofSeq

        let next () =
            match remaining with
            | [] -> None
            | x :: xs ->
                remaining <- xs |> List.filter (fun c -> not (x.Supersedes c))
                Some x

        seq {
            match next() with Some x -> yield x | None -> ()
            match next() with Some x -> yield x | None -> ()
            match next() with Some x -> yield x | None -> ()

            remaining <- remaining |> List.sortByDescending (fun c -> c.Rating)

            match next() with Some x -> yield x | None -> ()
            match next() with Some x -> yield x | None -> ()
            match next() with Some x -> yield x | None -> ()
        }