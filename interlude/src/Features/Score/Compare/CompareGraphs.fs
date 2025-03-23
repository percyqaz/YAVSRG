namespace Interlude.Features.Score

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude
open Prelude.Data.User
open Interlude.Content
open Interlude.UI
open Interlude.Features.Collections

type CompareGraphs(score_a: ScoreInfo, score_b: ScoreInfo, graph_a: ScoreGraph, graph_b: ScoreGraph, refresh: unit -> unit) =
    inherit Container(NodeType.None)

    let open_graph_settings () =
        ScoreGraphSettingsPage(
            max graph_a.Keys graph_b.Keys,
            fun column_filter_changed ->
                if column_filter_changed then
                    graph_a.ApplyColumnFilter()
                    graph_b.ApplyColumnFilter()
                graph_a.Refresh()
                graph_b.Refresh()
        )
            .Show()

    override this.Init(parent) =
        graph_a.OnRightClick <- open_graph_settings
        graph_b.OnRightClick <- open_graph_settings

        this
        |+ (
            GridFlowContainer<Widget>(50.0f, 3, Spacing = (30.0f, 0.0f), Position = { Position.SliceB(65.0f) with Left = 0.2f %+ 30.0f; Right = 0.8f %- 30.0f }.Translate(0.0f, 5.0f))
            |+ InlaidButton(
                %"score.graph.settings",
                open_graph_settings,
                Icons.EDIT_2
            )
            |+ InlaidButton(
                %"score.chart_actions",
                (fun () -> ScoreChartContextMenu(score_a).Show()),
                Icons.SETTINGS,
                Hotkey = "context_menu"
            )
            |+ RulesetSwitcher(
                Setting.simple Rulesets.selected_id.Value
                |> Setting.trigger (fun id ->
                    let ruleset = Rulesets.by_id id
                    score_a.Ruleset <- ruleset
                    score_b.Ruleset <- ruleset
                    refresh ()
                ),
                (fun ruleset ->
                    score_a.Ruleset <- ruleset
                    score_b.Ruleset <- ruleset
                    refresh()),
                score_a
            )
        )
        |* HotkeyAction("like", fun () -> CollectionActions.toggle_liked score_a.ChartMeta)

        base.Init parent

        graph_a.Init this
        graph_b.Init this

    override this.Draw () =
        // prevents hover info from being overlapped by the other graph
        if Mouse.y() < this.Bounds.CenterY then
            graph_b.Draw()
            graph_a.Draw()
        else
            graph_a.Draw()
            graph_b.Draw()

        base.Draw()

    override this.Update (elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        graph_a.Update(elapsed_ms, moved)
        graph_b.Update(elapsed_ms, moved)

        for k = 0 to (max graph_a.Keys graph_b.Keys) - 1 do
            if GraphSettings.COLUMN_FILTER_KEYS.[k].Tapped() then
                GraphSettings.column_filter.[k] <- not GraphSettings.column_filter.[k]
                graph_a.ApplyColumnFilter()
                graph_b.ApplyColumnFilter()
                graph_a.Refresh()
                graph_b.Refresh()