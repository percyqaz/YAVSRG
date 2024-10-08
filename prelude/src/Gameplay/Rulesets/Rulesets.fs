﻿namespace Prelude.Gameplay

open System
open Percyqaz.Common
open Percyqaz.Data
open Prelude

type JudgementId = int

type AccuracySystemState =
    {
        Judgements: int array
        mutable PointsScored: float
        mutable MaxPointsScored: float
        mutable CurrentCombo: int
        mutable BestCombo: int
        mutable MaxPossibleCombo: int
        mutable ComboBreaks: int
        mutable Sxx: float32<ms^2>
        mutable Sx: Time
        mutable N: float32
    }
    member this.BreakCombo(would_have_increased_combo: bool) =
        if would_have_increased_combo then
            this.MaxPossibleCombo <- this.MaxPossibleCombo + 1

        this.CurrentCombo <- 0
        this.ComboBreaks <- this.ComboBreaks + 1

    member this.IncrCombo() =
        this.MaxPossibleCombo <- this.MaxPossibleCombo + 1
        this.CurrentCombo <- this.CurrentCombo + 1
        this.BestCombo <- max this.CurrentCombo this.BestCombo

    member this.Add(points: float, maxpoints: float, judge: JudgementId) =
        this.PointsScored <- this.PointsScored + points
        this.MaxPointsScored <- this.MaxPointsScored + maxpoints
        this.Judgements.[judge] <- this.Judgements.[judge] + 1

    member this.AddDelta(ms_delta: Time) =
        this.N <- this.N + 1.0f
        this.Sx <- this.Sx + ms_delta
        this.Sxx <- this.Sxx + ms_delta * ms_delta

    member this.Mean = if this.N = 0.0f then 0.0f<ms> else this.Sx / this.N
    member this.StandardDeviation =
        let mean = this.Mean
        if this.N = 0.0f then 0.0f<ms> else MathF.Sqrt(this.Sxx / this.N - mean * mean |> float32) * 1.0f<ms>

/// Judgements are an indicator of how good a hit was, like "Perfect!" or "Nearly!"
/// Scores are commonly measured by how many of each judgement you get (for example a good score might be getting all "Perfect!" judgements)
[<Json.AutoCodec>]
type Judgement =
    {
        Name: string
        Color: Color
        BreaksCombo: bool
    }

/// Assignment of points per hit
/// Your total points are the sum of the points for each note you hit
/// Your % accuracy is number of points you get / max points possible
[<RequireQualifiedAccess>]
[<Json.AutoCodec>]
type AccuracyPoints =
    | WifeCurve of judge: int
    | Weights of maxweight: float * weights: float array
    member this.Validate name jcount =
        match this with
        | Weights(_, w) ->
            if w.Length <> jcount then
                Logging.Error(
                    sprintf
                        "Problem with ruleset '%s': %i accuracy weights given for %i judgements"
                        name
                        w.Length
                        jcount
                )

            this
        | _ -> this

[<Json.AutoCodec>]
type OsuLnWindows =
    {
        Window320: Time
        Window300: Time
        Window200: Time
        Window100: Time
        Window50: Time
        WindowOverhold200: Time
        WindowOverhold100: Time
    }

/// Behaviour for hold notes
[<RequireQualifiedAccess>]
[<Json.AutoCodec>]
type HoldNoteBehaviour =
    | Osu of OsuLnWindows
    | JustBreakCombo
    | Normal of
        {|
            JudgementIfDropped: JudgementId
            JudgementIfOverheld: JudgementId
        |}
    | JudgeReleases of
        {|
            Timegates: (Time * JudgementId) list
        |}
    | OnlyJudgeReleases of judgement_if_dropped: JudgementId // uses base timegates
    member this.Validate name jcount =
        match this with
        | Normal d ->
            if d.JudgementIfDropped >= jcount || d.JudgementIfDropped < 0 then
                Logging.Error(sprintf "Problem with ruleset '%s': JudgementIfDropped is not a valid judgement" name)

            if d.JudgementIfOverheld >= jcount || d.JudgementIfOverheld < 0 then
                Logging.Error(sprintf "Problem with ruleset '%s': JudgementIfOverheld is not a valid judgement" name)

            Normal d
        | JudgeReleases d ->
            let mutable last_time = -Time.infinity

            for (time, j) in d.Timegates do
                if time <= last_time then
                    Logging.Error(sprintf "Problem with ruleset '%s': Release timegates are in the wrong order" name)

                if j >= jcount || j < 0 then
                    Logging.Error(sprintf "Problem with ruleset '%s': Release timegates judgement is not valid" name)

                last_time <- time

            JudgeReleases d
        | _ -> this // todo: validate osu windows

/// Grades are awarded at the end of a score as a summarising "rank" of how well you did
/// They typically follow lettering systems similar to academic exam grades
[<Json.AutoCodec>]
type Grade =
    {
        Name: string
        Accuracy: float
        Color: Color
    }

module Grade =

    type GradeResult =
        {
            Grade: int
            /// Improvement needed to get next grade; None if you got the best grade
            AccuracyNeeded: float option
        }

    let calculate_with_target (grades: Grade array) (percent: float) : GradeResult =

        let rec loop (achieved: int) =
            if achieved + 1 = grades.Length then // got max grade already
                {
                    Grade = achieved
                    AccuracyNeeded = None
                }
            else
                let accuracy_needed = grades.[achieved + 1].Accuracy - percent

                if accuracy_needed > 0.0 then
                    {
                        Grade = achieved
                        AccuracyNeeded = Some accuracy_needed
                    }
                else
                    loop (achieved + 1)

        loop -1

    let calculate (grades: Grade array) (state: AccuracySystemState) =
        (calculate_with_target grades (state.PointsScored / state.MaxPointsScored))
            .Grade

/// Lamps are awarded at the end of the score as a summarising "tag" to indicate certain accomplishments
/// Examples: You didn't miss a single note, so you get a "Full Combo" tag, you only got "Perfect" judgements, so you get a "Perfect Full Combo" tag
/// These provide alternative accomplishments to grades that can provide different challenge
[<Json.AutoCodec>]
type Lamp =
    {
        Name: string
        Judgement: JudgementId
        JudgementThreshold: int
        Color: Color
    }

module Lamp =

    type LampResult =
        {
            Lamp: int
            /// Improvement needed to get next lamp; None if you got the best lamp
            ImprovementNeeded:
                {|
                    Judgement: JudgementId
                    LessNeeded: int
                |} option
        }

    let calculate_with_target (lamps: Lamp array) (state: AccuracySystemState) : LampResult =

        let worst_judgement =
            let mutable w = -1
            let mutable i = 0

            while i < state.Judgements.Length do
                if state.Judgements.[i] > 0 then
                    w <- i

                i <- i + 1

            w

        let rec loop (achieved: int) =
            if achieved + 1 = lamps.Length then // got max grade already
                {
                    Lamp = achieved
                    ImprovementNeeded = None
                }
            else
                let next_lamp = lamps.[achieved + 1]

                if next_lamp.Judgement < 0 then // then it refers to cbs
                    if state.ComboBreaks > next_lamp.JudgementThreshold then
                        {
                            Lamp = achieved
                            ImprovementNeeded =
                                Some
                                    {|
                                        Judgement = -1
                                        LessNeeded = state.ComboBreaks - next_lamp.JudgementThreshold
                                    |}
                        }
                    else
                        loop (achieved + 1)
                else if worst_judgement > next_lamp.Judgement then
                    {
                        Lamp = achieved
                        ImprovementNeeded =
                            Some
                                {|
                                    Judgement = worst_judgement
                                    LessNeeded = state.Judgements.[worst_judgement]
                                |}
                    }
                elif state.Judgements.[next_lamp.Judgement] > next_lamp.JudgementThreshold then
                    {
                        Lamp = achieved
                        ImprovementNeeded =
                            Some
                                {|
                                    Judgement = next_lamp.Judgement
                                    LessNeeded = state.Judgements.[next_lamp.Judgement] - next_lamp.JudgementThreshold
                                |}
                    }
                else
                    loop (achieved + 1)

        loop -1

    let calculate (lamps: Lamp array) (state: AccuracySystemState) : int =
        (calculate_with_target lamps state).Lamp

[<Json.AutoCodec>]
type GradingConfig =
    {
        Grades: Grade array
        Lamps: Lamp array
    }
    member this.Validate jcount = this // todo: could validate lamps against judgements

[<Json.AutoCodec>]
type AccuracyConfig =
    {
        MissWindow: Time
        CbrushWindow: Time
        Timegates: (Time * JudgementId) list
        Points: AccuracyPoints
        HoldNoteBehaviour: HoldNoteBehaviour
    }
    member this.Validate name jcount =
        { this with
            Timegates =
                let mutable last_time = -Time.infinity

                for (time, j) in this.Timegates do
                    if time <= last_time then
                        Logging.Error(sprintf "Problem with ruleset '%s': Timegates are in the wrong order" name)

                    if j >= jcount || j < 0 then
                        Logging.Error(sprintf "Problem with ruleset '%s': Timegates judgement is not valid" name)

                    last_time <- time

                this.Timegates
            Points = this.Points.Validate name jcount
            HoldNoteBehaviour = this.HoldNoteBehaviour.Validate name jcount
        }

[<Json.AutoCodec>]
type Ruleset =
    {
        Name: string
        Description: string

        Judgements: Judgement array
        Accuracy: AccuracyConfig
        Grading: GradingConfig
    }
    member this.DefaultJudgement: JudgementId = this.Judgements.Length - 1

    member this.GradeName i =
        if i < 0 then "F"
        else if i >= this.Grading.Grades.Length then "??"
        else this.Grading.Grades.[i].Name

    member this.GradeColor i =
        if i < 0 || i >= this.Grading.Grades.Length then
            Color.Gray
        else
            this.Grading.Grades.[i].Color

    member this.LampName i =
        if i < 0 then "NONE"
        else if i >= this.Grading.Lamps.Length then "??"
        else this.Grading.Lamps.[i].Name

    member this.LampColor i =
        if i < 0 || i >= this.Grading.Grades.Length then
            Color.White
        else
            this.Grading.Lamps.[i].Color

    member this.JudgementName i = this.Judgements.[i].Name
    member this.JudgementColor i = this.Judgements.[i].Color

    member this.Validate =
        { this with
            Accuracy = this.Accuracy.Validate this.Name this.Judgements.Length
            Grading = this.Grading.Validate this.Judgements.Length
        }

module Ruleset =

    open System.IO
    open System.Security.Cryptography

    let hash (config: Ruleset) =
        let h = SHA256.Create()
        use ms = new MemoryStream()
        use bw = new BinaryWriter(ms)

        for j in config.Judgements do
            bw.Write j.BreaksCombo

        bw.Write(float32 config.Accuracy.MissWindow)
        bw.Write(float32 config.Accuracy.CbrushWindow)

        for t, j in config.Accuracy.Timegates do
            bw.Write(float32 t)
            bw.Write j

        match config.Accuracy.Points with
        | AccuracyPoints.WifeCurve j -> bw.Write j
        | AccuracyPoints.Weights(max, pts) ->
            bw.Write max

            for p in pts do
                bw.Write p

        match config.Accuracy.HoldNoteBehaviour with
        | HoldNoteBehaviour.Osu windows -> 
            bw.Write (float32 windows.Window320)
            bw.Write (float32 windows.Window300)
            bw.Write (float32 windows.Window200)
            bw.Write (float32 windows.Window100)
            bw.Write (float32 windows.Window50)
            bw.Write (float32 windows.WindowOverhold200)
            bw.Write (float32 windows.WindowOverhold100)
        | HoldNoteBehaviour.JustBreakCombo -> bw.Write 0s
        | HoldNoteBehaviour.Normal rules ->
            bw.Write rules.JudgementIfDropped
            bw.Write rules.JudgementIfOverheld
        | HoldNoteBehaviour.JudgeReleases d ->
            for t, j in d.Timegates do
                bw.Write(float32 t)
                bw.Write j
        | HoldNoteBehaviour.OnlyJudgeReleases j -> bw.Write j

        for g in config.Grading.Grades do
            bw.Write g.Accuracy

        for l in config.Grading.Lamps do
            bw.Write l.Judgement
            bw.Write l.JudgementThreshold

        let s = ms.ToArray() |> h.ComputeHash |> BitConverter.ToString
        config.Name.Replace(" ", "") + s.Replace("-", "").Substring(0, 6)