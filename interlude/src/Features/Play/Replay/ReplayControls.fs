﻿namespace Interlude.Features.Play.Replay

open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Percyqaz.Common
open Prelude
open Prelude.Charts.Processing
open Interlude.UI
open Interlude.Features.Gameplay

type private ReplayModeSettingsPage() =
    inherit Page()

    override this.Content() =
        page_container()
        |+ PageSetting(%"replay.input_overlay", Checkbox show_input_overlay).Pos(0)
        |+ PageSetting(%"replay.hit_overlay", Checkbox show_hit_overlay).Pos(2)
        |+ PageSetting(%"replay.playfield_dim", Slider.Percent playfield_dim)
            .Conditional(fun () -> show_input_overlay.Value || show_hit_overlay.Value)
            .Pos(4)
        :> Widget

    override this.Title = sprintf "%s %s" Icons.SETTINGS (%"replay.settings")
    override this.OnClose() = ()

type private ReplayControls(with_mods: ModdedChart, is_auto: bool, rate: float32, on_seek: Time -> unit) =
    inherit Container(NodeType.None)

    let fade = Animation.Fade(1.0f)
    let mutable auto_hide_timer = 3000.0
    let mutable show_cooldown = 0.0
    
    let playback_speed = 
        Setting.bounded rate 0.25f 3.0f
        |> Setting.trigger (fun r -> 
            Song.change_rate r
            fade.Target <- 1.0f
            Toolbar.show_cursor ()
            auto_hide_timer <- 1500.0
        )

    override this.Init(parent) =
        this
        |+ Text(
            Icons.FILM + " " + (if is_auto then %"replay.title.autoplay" else %"replay.title"),
            Position = Position.SliceT(90.0f).ShrinkX(25.0f).TranslateY(10.0f),
            Align = Alignment.LEFT
        )
        |+ PageButton(
            sprintf "%s %s" Icons.SETTINGS (%"replay.settings"), 
            (fun () -> ReplayModeSettingsPage().Show()),
            Position = Position.SliceT(50.0f).SliceL(500.0f).ShrinkX(25.0f).TranslateY(105.0f).Expand(Style.PADDING)
        )
        |+ Text(
            sprintf "%s: %O" (%"replay.hide_overlay") (%%"hide_replay_overlay"),
            Position = Position.SliceT(50.0f).ShrinkX(25.0f).TranslateY(160.0f),
            Color = K Colors.text_cyan,
            Align = Alignment.LEFT
        )
        
        |+ Text(
            (fun () -> sprintf "%s %.2fx" Icons.FAST_FORWARD playback_speed.Value),
            Position = Position.SliceT(90.0f).ShrinkX(25.0f).TranslateY(10.0f),
            Align = Alignment.RIGHT
        )
        |+ Text(
            (fun () -> sprintf "%s: %.2fx" (%"replay.original_rate") rate),
            Position = Position.SliceT(50.0f).ShrinkX(25.0f).TranslateY(105.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT
        )
        |+ Text(
            sprintf "%s: %O/%O" (%"replay.change_playback_rate") (%%"uprate") (%%"downrate"),
            Position = Position.SliceT(50.0f).ShrinkX(25.0f).TranslateY(160.0f),
            Color = K Colors.text_cyan,
            Align = Alignment.RIGHT
        )
        |+ Text(
            sprintf "%s: %O" (%"replay.pause") (%%"skip"),
            Position = Position.SliceT(50.0f).ShrinkX(25.0f).TranslateY(210.0f),
            Color = K Colors.text_cyan,
            Align = Alignment.RIGHT
        )

        |* Timeline(with_mods, on_seek, SelectedChart.rate)

        base.Init parent

    override this.Draw() =
        if fade.Alpha > 0 then
            let old_m = Alpha.change_multiplier fade.Value
            base.Draw()
            Alpha.change_multiplier old_m |> ignore

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        fade.Update elapsed_ms

        if show_cooldown <= 0.0 && Mouse.moved_recently () then
            fade.Target <- 1.0f
            Toolbar.show_cursor ()
            auto_hide_timer <- 1500.0

        elif fade.Target = 1.0f && not (Dialog.exists()) then
            auto_hide_timer <- auto_hide_timer - elapsed_ms

            if auto_hide_timer <= 0.0 then
                fade.Target <- 0.0f
                Toolbar.hide_cursor ()

            if (%%"hide_replay_overlay").Pressed() then
                fade.Target <- 0.0f
                Toolbar.hide_cursor ()
                show_cooldown <- 1000.0

        else show_cooldown <- show_cooldown - elapsed_ms

        if (%%"skip").Tapped() then
            if Song.playing () then 
                (if Song.time () > 0.0f<ms> then Song.pause ())
            elif not (Mouse.held Mouse.LEFT) then Song.resume ()
        else 
            SelectedChart.change_rate_hotkeys (fun change_by -> playback_speed.Value <- playback_speed.Value + change_by)