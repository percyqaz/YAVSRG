namespace Interlude.Features.Score

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude
open Prelude.Gameplay.Scoring
open Prelude.Data.User
open Interlude.UI

#nowarn "3370"

type CompareAccuracy(stats: ScoreScreenStats ref, score_info: ScoreInfo) =
    inherit StaticWidget(NodeType.None)

    let mutable hover = false

    override this.Update(elapsed_ms, moved) =
        hover <- Mouse.hover this.Bounds
        base.Update(elapsed_ms, moved)

    override this.Draw() =
        Render.rect (this.Bounds.Translate(10.0f, 10.0f)) Colors.black
        Background.draw (this.Bounds, (Color.FromArgb(40, 40, 40)), 2.0f)
        let grade_color = score_info.Ruleset.GradeColor score_info.Grade
        Render.rect this.Bounds grade_color.O1

        if (!stats).ColumnFilterApplied then
            Text.fill_b (
                Style.font,
                score_info.Scoring.Ruleset.FormatAccuracy (!stats).Accuracy,
                this.Bounds.Shrink(10.0f, 0.0f),
                Colors.text_green,
                Alignment.CENTER
            )
        else
            Text.fill_b (
                Style.font,
                score_info.Scoring.FormattedAccuracy,
                this.Bounds.Shrink(10.0f, 0.0f),
                (grade_color, Colors.black),
                Alignment.CENTER
            )

        if hover then
            let acc_tooltip = this.Bounds.SliceX(150.0f).BorderT(60.0f).TranslateY(-15.0f)
            Render.rect (acc_tooltip.Expand(Style.PADDING)) Colors.white
            Render.rect acc_tooltip Colors.shadow_2

            Text.fill_b (
                Style.font,
                sprintf "%.4f%%" (stats.Value.Accuracy * 100.0),
                acc_tooltip.Shrink(10.0f, 5.0f),
                (if stats.Value.ColumnFilterApplied then Colors.text_green else Colors.text),
                Alignment.CENTER
            )

type CompareLamp(score_info: ScoreInfo) =
    inherit Container(NodeType.None)

    let mutable hover = false

    override this.Init(parent) =
        this
        |* Text(
            (fun () -> score_info.Ruleset.LampName score_info.Lamp),
            Color = (fun () -> (score_info.Ruleset.LampColor score_info.Lamp, Colors.black)),
            Position = Position.Shrink(10.0f, 0.0f)
        )

        base.Init parent

    override this.Update(elapsed_ms, moved) =
        hover <- Mouse.hover this.Bounds
        base.Update(elapsed_ms, moved)

    override this.Draw() =
        Render.rect (this.Bounds.Translate(10.0f, 10.0f)) Colors.black
        Background.draw (this.Bounds, (Color.FromArgb(40, 40, 40)), 2.0f)
        Render.rect this.Bounds (score_info.Ruleset.LampColor score_info.Lamp).O1
        base.Draw()

        if hover then
            let raw_greats_tooltip = this.Bounds.SliceX(150.0f).BorderT(60.0f).TranslateY(-15.0f)
            Render.rect (raw_greats_tooltip.Expand(Style.PADDING)) Colors.white
            Render.rect raw_greats_tooltip Colors.shadow_2

            Text.fill_b (
                Style.font,
                sprintf "%.1f raw" (score_info.Scoring.MaxPossiblePoints - score_info.Scoring.PointsScored),
                raw_greats_tooltip.Shrink(10.0f, 5.0f),
                Colors.text,
                Alignment.CENTER
            )