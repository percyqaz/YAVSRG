namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Calculator.Patterns
open Prelude.Calculator
open Interlude.UI
open Interlude.Features.Gameplay

type Patterns(display: Setting<InfoPanelMode>) =
    inherit Container(NodeType.None)

    let mutable patterns: PatternCluster array = [||]
    let mutable category: string = ""

    let on_chart_update(info: LoadedChartInfo) =
        patterns <- info.Patterns.MainPatterns
        category <- sprintf "%.2f%% S | %.2f%% P" (info.Patterns.Simplicity * 100.0f) (info.Patterns.Purity * 100.0f)

    override this.Init(parent: Widget) =
        base.Init parent
        SelectedChart.on_chart_change_finished.Add on_chart_update
        SelectedChart.when_loaded false on_chart_update

        this
        |* AngledButton(
            %"levelselect.info.details",
            (fun () -> display.Set InfoPanelMode.Local),
            Palette.MAIN_100
        )
            .Hotkey("scoreboard_storage")
            .LeanLeft(false)
            .LeanRight(false)
            .Position(Position.SliceT(AngledButton.HEIGHT))
            .Help(Help.Info("levelselect.info.mode", "scoreboard_storage"))

    override this.Draw() =
        base.Draw()

        let mutable b =
            this.Bounds.SliceT(60.0f, 65.0f).ShrinkX(20.0f)

        let TEXT_WIDTH = 360.0f

        for entry in patterns do
            Text.fill_b (
                Style.font,
                (sprintf "%O" entry.Pattern),
                b.ShrinkB(25.0f).SliceL(TEXT_WIDTH),
                Colors.text,
                Alignment.LEFT
            )

            Text.fill_b (
                Style.font,
                String.concat ", " (entry.SpecificPatterns |> Seq.truncate 2 |> Seq.map (fun (p, amount) -> sprintf "%.0f%% %s" (amount * 100.0f) p)),
                b.SliceB(30.0f).SliceL(TEXT_WIDTH),
                Colors.text_subheading,
                Alignment.LEFT
            )

            Text.fill_b (
                Style.font,
                Icons.MUSIC + " " + (match entry.Type with ClusterType.Normal bpm -> sprintf "%i" bpm | ClusterType.Mixed bpm -> sprintf "~%i" bpm | ClusterType.Combined (min, max) -> sprintf "%i-%i" min max),
                b.ShrinkL(TEXT_WIDTH).ShrinkY(10.0f),
                Colors.text,
                Alignment.LEFT
            )

            Text.fill_b (
                Style.font,
                Icons.STAR + " " + (sprintf "%.2f" entry.Rating),
                b.ShrinkL(TEXT_WIDTH).ShrinkY(10.0f),
                Colors.text,
                Alignment.CENTER
            )

            Text.fill_b (
                Style.font,
                Icons.CLOCK + " " + (format_duration_ms (entry.Amount / SelectedChart.rate.Value)),
                b.ShrinkL(TEXT_WIDTH).ShrinkY(10.0f),
                Colors.text,
                Alignment.RIGHT
            )

            b <- b.Translate(0.0f, 65.0f)

        Text.fill_b (Style.font, category, this.Bounds.SliceB(60.0f), Colors.text, Alignment.CENTER)