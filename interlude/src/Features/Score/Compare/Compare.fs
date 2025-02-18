namespace Interlude.Features.Score

open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
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
        |+ TopBanner(score_a, Position = Position.SliceT(180.0f))
        |+ Sidebar(
            stats_a,
            score_a,
            Position =
                {
                    Left = 0.0f %+ 20.0f
                    Top = 0.0f %+ 215.0f
                    Right = 0.2f %- 0.0f
                    Bottom = 1.0f %- 70.0f
                }
        )
        |+ Sidebar(
            stats_b,
            score_b,
            Position =
                {
                    Left = 0.8f %- 0.0f
                    Top = 0.0f %+ 215.0f
                    Right = 1.0f %- 20.0f
                    Bottom = 1.0f %- 70.0f
                }
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

    override this.Update(elapsed_ms, moved) =
        ScoreScreenHelpers.animation_queue.Update elapsed_ms
        base.Update(elapsed_ms, moved)

    override this.OnEnter prev =
        Toolbar.hide ()
        DiscordRPC.in_menus ("Comparing two scores")

    override this.OnExit next =
        score_a.Ruleset <- Rulesets.current
        score_b.Ruleset <- Rulesets.current // todo: put it back to the original, which may not be current if leaderboard
        (graph_a :> System.IDisposable).Dispose()
        (graph_b :> System.IDisposable).Dispose()
        on_ruleset_changed.Dispose()
        Toolbar.show ()

    override this.OnBack() =
        if Network.lobby.IsSome then
            Some Screen.Type.Lobby
        else
            Some Screen.Type.LevelSelect

    override this.Draw() =

        Render.rect (this.Bounds.ShrinkT(175.0f).SliceT(160.0f).ShrinkT(5.0f)) Colors.shadow_2.O2
        Render.rect (this.Bounds.ShrinkT(175.0f).ShrinkT(160.0f).SliceT(5.0f)) Colors.white

        base.Draw()