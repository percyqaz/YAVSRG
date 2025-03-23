namespace Interlude.Features.Score

open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude.Gameplay.Scoring
open Prelude.Data.User
open Interlude.Content
open Interlude.UI
open Interlude.Features.Online

#nowarn "3370"

type ScoreCompareScreen(score_a: ScoreInfo, score_b: ScoreInfo) =
    inherit Screen()

    let stats_a = ref <| ScoreScreenStats.calculate score_a.Scoring GraphSettings.column_filter
    let stats_b = ref <| ScoreScreenStats.calculate score_b.Scoring GraphSettings.column_filter
    let graph_a = new ScoreGraph(score_a, stats_a, CompareA)
    let graph_b = new ScoreGraph(score_b, stats_b, CompareB)

    let refresh () =
        stats_a := ScoreScreenStats.calculate score_a.Scoring GraphSettings.column_filter
        stats_b := ScoreScreenStats.calculate score_b.Scoring GraphSettings.column_filter
        graph_a.Refresh()
        graph_b.Refresh()

    let on_ruleset_changed = Rulesets.on_changed.Subscribe (fun _ -> GameThread.defer refresh)

    override this.Init(parent) =
        this
        |+ CompareBanner(score_a, score_b, Position = Position.SliceT(180.0f))
        |+ ComparePanel(
            stats_a,
            score_a,
            false,
            Position =
                Position
                    .SlicePercentL(0.2f)
                    .ShrinkL(20.0f)
                    .ShrinkT(215.0f)
                    .ShrinkB(260.0f)
        )
        |+ CompareLamp(
            score_a,
            Position =
                Position
                    .SlicePercentL(0.2f)
                    .ShrinkL(20.0f)
                    .SliceB(140.0f, 100.0f)
        )
        |+ CompareAccuracy(
            stats_a,
            score_a,
            Position =
                Position
                    .SlicePercentL(0.2f)
                    .ShrinkL(20.0f)
                    .SliceB(20.0f, 100.0f)
        )
        |+ ComparePanel(
            stats_b,
            score_b,
            true,
            Position =
                Position
                    .SlicePercentR(0.2f)
                    .ShrinkR(20.0f)
                    .ShrinkT(215.0f)
                    .ShrinkB(260.0f)
        )
        |+ CompareLamp(
            score_b,
            Position =
                Position
                    .SlicePercentR(0.2f)
                    .ShrinkR(20.0f)
                    .SliceB(140.0f, 100.0f)
        )
        |+ CompareAccuracy(
            stats_b,
            score_b,
            Position =
                Position
                    .SlicePercentR(0.2f)
                    .ShrinkR(20.0f)
                    .SliceB(20.0f, 100.0f)
        )
        |+ CompareGraphs(
            score_a,
            score_b,
            graph_a,
            graph_b,
            refresh,
            Position =
                { Position.DEFAULT with
                    Top = 0.0f %+ 190.0f
                }
        )
        |* Confetti()

        base.Init parent

    override this.OnEnter prev =
        Toolbar.hide ()
        DiscordRPC.in_menus "Comparing two scores"

    override this.OnExit next =
        score_a.Ruleset <- Rulesets.current
        score_b.Ruleset <- Rulesets.current // todo: put it back to the original, which may not be current if leaderboard
        (graph_a :> System.IDisposable).Dispose()
        (graph_b :> System.IDisposable).Dispose()
        on_ruleset_changed.Dispose()
        Toolbar.show ()

    override this.OnBack() =
        if Network.lobby.IsSome then Some ScreenType.Lobby else Some ScreenType.LevelSelect