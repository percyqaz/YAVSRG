namespace Interlude.Features.Play

open Percyqaz.Flux.Audio
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Prelude.Mods
open Prelude.Calculator.Patterns
open Interlude.Options
open Interlude.Features.Gameplay
open Interlude.Features.Play

type PatternsOverlay(chart: ModdedChart, playfield: Playfield, patterns: PatternInfo, state: PlayState) =
    inherit StaticWidget(NodeType.None)

    let mutable segment_seek = 0
    let mutable last_time = state.CurrentChartTime()

    let scroll_direction_pos: float32 -> float32 -> float32 =
        if options.Upscroll.Value then
            fun _ -> id
        else
            fun bottom -> fun x -> bottom - x

    let draw_segment (now: Time, t_start: Time, t_end: Time, s_type: SegmentType) =
        let a =
            options.HitPosition.Value
            + (t_start - now) * (options.ScrollSpeed.Value / SelectedChart.rate.Value)
            + playfield.ColumnWidth * 0.5f + 5.0f
            |> scroll_direction_pos playfield.Bounds.Bottom

        let b =
            options.HitPosition.Value
            + (t_end - now) * (options.ScrollSpeed.Value / SelectedChart.rate.Value)
            + playfield.ColumnWidth * 0.5f - 5.0f
            |> scroll_direction_pos playfield.Bounds.Bottom

        let segment_area = Rect.FromEdges(playfield.Bounds.Left - 40.0f, a, playfield.Bounds.Left - 10.0f, b)

        let color =
            match s_type with
            | SegmentType.Jacks _ -> Colors.red_accent
            | SegmentType.Chordstream _ -> Colors.green_accent
            | SegmentType.Stream _ -> Colors.cyan_accent
            | SegmentType.Uncategorized -> Colors.yellow_accent

        Render.rect segment_area color.O3

    let draw_performance_data (y: float32) (color: Color) (data: float32 seq) =
        let mutable x = 20.0f
        for d in Seq.truncate 100 data do
            Render.rect_size (x - 5.0f) y 5.0f (d * 0.5f) color
            x <- x + 5.0f

    override this.Draw() =
        let now =
            state.CurrentChartTime() +
            (GameThread.frame_compensation () + options.VisualOffset.Value) * Song.playback_rate()

        while patterns.Segments.Length - 1 > segment_seek && patterns.Segments.[segment_seek + 1].Start < now do
            segment_seek <- segment_seek + 1

        let until_time =
            now + 1080.0f / (options.ScrollSpeed.Value / SelectedChart.rate.Value)

        let mutable segment_peek = segment_seek
        let mutable current_segment = patterns.Segments.[segment_seek]

        Text.draw(Style.font, sprintf "%A" current_segment.Type, 20.0f, 200.0f, 70.0f, Colors.white)

        draw_segment(now, current_segment.Start, current_segment.End, current_segment.Type)

        while patterns.Segments.Length - 1 > segment_peek && patterns.Segments.[segment_peek].Start < until_time do
            segment_peek <- segment_peek + 1
            current_segment <-  patterns.Segments.[segment_peek]
            draw_segment(now, current_segment.Start, current_segment.End, current_segment.Type)

    override this.Update (elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        let time = state.CurrentChartTime()
        if time < last_time then
            while segment_seek > 0 && patterns.Segments.[segment_seek].Start > time do
                segment_seek <- segment_seek - 1

        last_time <- time