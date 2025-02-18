namespace Interlude.Features.Score

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.User
open Interlude.UI

type CompareBanner(score_a: ScoreInfo, score_b: ScoreInfo) =
    inherit Container(NodeType.None)

    let compare_string_a =
        sprintf "Comparing: Score%s at %s"
            (match score_a.PlayedBy with ScorePlayedBy.You -> "" | ScorePlayedBy.Username p -> sprintf " by %s" p)
            ((score_a.TimePlayed |> Timestamp.to_datetime).ToLocalTime().ToString())

    let compare_string_b =
        sprintf "To: Score%s at %s"
            (match score_b.PlayedBy with ScorePlayedBy.You -> "" | ScorePlayedBy.Username p -> sprintf " by %s" p)
            ((score_b.TimePlayed |> Timestamp.to_datetime).ToLocalTime().ToString())

    override this.Init(parent) =
        this
        |+ Text(
            score_a.ChartMeta.Artist + " - " + score_a.ChartMeta.Title,
            Align = Alignment.LEFT,
            Position = Position.SliceT(85.0f).ShrinkX(20.0f).ShrinkR(300.0f)
        )
        |+ Text(
            score_a.ChartMeta.DifficultyName,
            Align = Alignment.LEFT,
            Position = Position.SliceT(75.0f, 55.0f).ShrinkX(20.0f)
        )
        |+ Text(
            sprintf "%s  •  %s" ([score_a.ChartMeta.OriginString] %> "score.source") ([score_a.ChartMeta.Creator] %> "score.creator"),
            Align = Alignment.LEFT,
            Position = Position.SliceT(125.0f, 40.0f).ShrinkX(20.0f)
        )

        |+ Text(
            compare_string_a,
            Align = Alignment.RIGHT,
            Position = Position.SliceT(65.0f, 50.0f).ShrinkX(20.0f)
        )
        |* Text(
            compare_string_b,
            Align = Alignment.RIGHT,
            Position = Position.SliceT(115.0f, 50.0f).ShrinkX(20.0f)
        )

        match score_a.PlayedBy with
        | ScorePlayedBy.You ->
            this |* Username(Position = Position.SliceT(Toolbar.HEIGHT).SliceR(300.0f))
        | _ -> ()

        base.Init parent

    override this.Draw() =

        Render.rect (this.Bounds.ShrinkB 5.0f) (Palette.color (127, 0.5f, 0.0f))
        Render.rect (this.Bounds.SliceB 5.0f) Colors.white.O2

        base.Draw()