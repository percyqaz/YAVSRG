namespace Prelude.Calculator.Patterns

open System.IO
open Prelude
open Prelude.Calculator.Patterns

[<AutoOpen>]
module LibraryPatternInfoSerialization =

    type CorePattern with

        member this.Write (bw: BinaryWriter) =
            match this with
            | Jacks -> bw.Write 0uy
            | Chordstream -> bw.Write 1uy
            | Stream -> bw.Write 2uy

        static member Read (br: BinaryReader) : CorePattern =
            match br.ReadByte() with
            | 0uy -> Jacks
            | 1uy -> Chordstream
            | 2uy -> Stream
            | unexpected -> failwithf "Unexpected byte '%x'" unexpected

    type ChartTag with

        member this.Write (bw: BinaryWriter) =
            match this with
            | ChartTag.Jacks -> bw.Write 0x00uy
            | ChartTag.Chordstream -> bw.Write 0x01uy
            | ChartTag.Stream -> bw.Write 0x02uy
            | ChartTag.LN -> bw.Write 0x03uy
            | ChartTag.SV -> bw.Write 0x04uy
            | ChartTag.Pure -> bw.Write 0x05uy
            | ChartTag.Beginner -> bw.Write 0x06uy
            | ChartTag.Hybrid -> bw.Write 0x07uy

        static member Read (br: BinaryReader) : ChartTag =
            match br.ReadByte() with
            | 0x00uy -> ChartTag.Jacks
            | 0x01uy -> ChartTag.Chordstream
            | 0x02uy -> ChartTag.Stream
            | 0x03uy -> ChartTag.LN
            | 0x04uy -> ChartTag.SV
            | 0x05uy -> ChartTag.Pure
            | 0x06uy -> ChartTag.Beginner
            | 0x07uy -> ChartTag.Hybrid
            | unexpected -> failwithf "Unexpected tag '%d'" unexpected

    type LibraryCategoryInfo with

        member this.Write (bw: BinaryWriter) =
            bw.Write (float32 this.Amount)
            bw.Write (fst this.Difficulty)
            bw.Write (snd this.Difficulty)
            bw.Write this.Importance
            bw.Write this.SpecificBPMs.Length
            for bpm, time, (diff100, diff150) in this.SpecificBPMs do
                bw.Write (int bpm)
                bw.Write (float32 time)
                bw.Write diff100
                bw.Write diff150

        static member Read (br: BinaryReader) : LibraryCategoryInfo =
            {
                Amount = br.ReadSingle() * 1.0f<ms>
                Difficulty = br.ReadSingle(), br.ReadSingle()
                Importance = br.ReadSingle()
                SpecificBPMs =
                    Array.init
                    <| br.ReadInt32()
                    <| (fun _ ->
                        br.ReadInt32() * 1<beat / minute / rate>,
                        br.ReadSingle() * 1.0f<ms>,
                        (br.ReadSingle(), br.ReadSingle())
                    )
            }

    type LibraryUncategorisedInfo with

        member this.Write (bw: BinaryWriter) =
            bw.Write (float32 this.Amount)
            bw.Write (fst this.Difficulty)
            bw.Write (snd this.Difficulty)
            bw.Write this.Importance

        static member Read (br: BinaryReader) : LibraryUncategorisedInfo =
            {
                Amount = br.ReadSingle() * 1.0f<ms>
                Difficulty = br.ReadSingle(), br.ReadSingle()
                Importance = br.ReadSingle()
            }

    type LibraryPatternInfo with

        member this.Write (bw: BinaryWriter) : unit =
            bw.Write (fst this.Difficulty)
            bw.Write (snd this.Difficulty)
            bw.Write (float32 this.SVAmount)
            bw.Write this.HoldNotePercent
            bw.Write this.Purity

            this.Jacks.Write bw
            this.Chordstream.Write bw
            this.Stream.Write bw
            this.Uncategorised.Write bw

            bw.Write this.Tags.Count
            for tag in this.Tags do
                tag.Write bw

        static member Read (br: BinaryReader) : LibraryPatternInfo =
            {
                Difficulty = br.ReadSingle(), br.ReadSingle()
                SVAmount = br.ReadSingle() * 1.0f<ms>
                HoldNotePercent = br.ReadSingle()
                Purity = br.ReadSingle()

                Jacks = LibraryCategoryInfo.Read br
                Chordstream = LibraryCategoryInfo.Read br
                Stream = LibraryCategoryInfo.Read br
                Uncategorised = LibraryUncategorisedInfo.Read br
                Tags =
                    Array.init (br.ReadInt32()) (fun _ -> ChartTag.Read br)
                    |> Set.ofArray
            }