﻿namespace Interlude.Features.Score

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Rulesets
open Prelude.Data.User
open Prelude.Skins.Noteskins
open Interlude
open Interlude.Content
open Interlude.UI
open Interlude.Features.Rulesets
open Interlude.Features.Gameplay
open Interlude.Features.Collections
open Interlude.Features.Online

type RulesetSwitcher(setting: Setting<string>, set_ruleset_direct: Ruleset -> unit, score_info: ScoreInfo) =
    inherit Container(NodeType.None)

    let mutable dropdown_height = 500.0f

    let dropdown_wrapper = DropdownWrapper(fun d -> dropdown_height <- min d.Height 500.0f; Position.BorderT(dropdown_height).ShrinkX(Style.PADDING).TranslateY(-10.0f))

    let switch_to_native_ruleset () =
        match Rulesets.get_native_ruleset score_info.ChartMeta.Origins with
        | Some ruleset ->
            // Use user's version (custom colors, names, etc) if available
            Rulesets.by_hash (Ruleset.hash ruleset)
            |> Option.defaultValue ruleset
            |> set_ruleset_direct
            Style.click.Play()
        | None ->
            Notifications.system_feedback(Icons.HELP_CIRCLE, %"notification.no_native_ruleset.title", %"notification.no_native_ruleset.body")

    override this.Init(parent: Widget) =
        this
        |+ InlaidButton(
            (fun () -> score_info.Ruleset.Name),
            (fun () -> this.ToggleDropdown()),
            "",
            Hotkey = "ruleset_switch",
            HoverText = %"score.switch_ruleset"
        )
        |+ HotkeyAction("native_ruleset", switch_to_native_ruleset)
        |* dropdown_wrapper

        base.Init parent

    override this.Draw() =
        base.Draw()
        if dropdown_wrapper.Active && not score_info.ChartMeta.Origins.IsEmpty then
            Text.draw_aligned_b(Style.font, sprintf "%O: Use original ruleset" (%%"native_ruleset"), 20.0f, this.Bounds.Right - 10.0f, this.Bounds.Top - dropdown_height - 50.0f, Colors.text_cyan_2, Alignment.RIGHT)

    member this.ToggleDropdown() =
        RulesetSwitcher.make_dropdown setting dropdown_wrapper

type BottomBanner(score_info: ScoreInfo, played_just_now: bool, graph: ScoreGraph, refresh: unit -> unit) =
    inherit Container(NodeType.None)

    let open_graph_settings () =
        ScoreGraphSettingsPage(
            graph.Keys,
            fun column_filter_changed ->
                if column_filter_changed then graph.ApplyColumnFilter()
                graph.Refresh()
        )
            .Show()

    override this.Init(parent) =

        graph.OnRightClick <- open_graph_settings

        if Network.lobby.IsNone && played_just_now then
            this
            |+ StylishButton(
                Gameplay.continue_endless_mode >> ignore,
                K (sprintf "%s %s" Icons.PLAY %"score.continue"),
                !%Palette.MAIN.O2,
                TiltRight = false,
                Position = Position.BorderT(50.0f).SliceR(300.0f),
                Floating = true
            )
            |+ StylishButton(
                Gameplay.retry,
                K (sprintf "%s %s" Icons.REPEAT %"score.retry"),
                !%Palette.DARK.O2,
                Position = Position.BorderT(50.0f).SliceR(300.0f).Translate(-325.0f, 0.0f),
                Floating = true
            )
            |+ HotkeyAction("retry", Gameplay.retry)
            |* HotkeyAction("select", Gameplay.continue_endless_mode >> ignore)

        this
        |+ graph
        |+ Text(
            Updates.version + "  : :  www.yavsrg.net",
            Position = { Position.SliceB(40.0f) with Right = 0.35f %+ 0.0f }.ShrinkX(20.0f).TranslateY(-15.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.CENTER
        )
        |+ (
            GridFlowContainer<Widget>(50.0f, 4, Spacing = (30.0f, 0.0f), Position = { Position.SliceB(65.0f) with Left = 0.35f %+ 30.0f; Right = 1.0f %- 20.0f }.Translate(0.0f, 5.0f))
            |+ InlaidButton(
                %"score.graph.settings",
                open_graph_settings,
                Icons.EDIT_2
            )
            |+ InlaidButton(
                %"score.chart_actions",
                (fun () -> ScoreChartContextMenu(score_info).Show()),
                Icons.SETTINGS,
                Hotkey = "context_menu"
            )
            |+ InlaidButton(
                %"score.watch_replay",
                (fun () ->
                    Gameplay.watch_replay (score_info, NoteColors.apply Content.NoteskinConfig.NoteColors score_info.WithMods)
                ),
                Icons.FILM
            )
            |+ RulesetSwitcher(
                Setting.simple Rulesets.selected_id.Value
                |> Setting.trigger (fun id ->
                    score_info.Ruleset <- Rulesets.by_id id
                    refresh ()
                ),
                (fun ruleset -> score_info.Ruleset <- ruleset; refresh()),
                score_info
            )
        )
        |* HotkeyAction("like", fun () -> CollectionActions.toggle_liked score_info.ChartMeta)

        base.Init parent

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
        |+ graph_a
        |+ graph_b
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
                score_a
            )
        )
        |* HotkeyAction("like", fun () -> CollectionActions.toggle_liked score_a.ChartMeta)

        base.Init parent