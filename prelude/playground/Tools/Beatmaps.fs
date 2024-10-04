﻿module Beatmaps

open System.IO
open Prelude
open Prelude.Charts.Formats.osu

let main() =
    for folder in Directory.EnumerateDirectories(Data.Library.Imports.OSU_SONG_FOLDER) do
        for file in Directory.EnumerateFiles(folder) do
            if file.ToLowerInvariant().EndsWith(".osu") then
                File.Copy(file, "C:/Users/percy/Desktop/compare_original.osu", true)
                match Beatmap.FromFile(file) with
                | Ok b -> 
                    b.ToFile("C:/Users/percy/Desktop/compare_new.osu")
                    printfn "Converted %s.\nCheck files for correctness" file
                    System.Console.ReadLine() |> ignore
                | Error reason -> failwithf "Failed to parse %s: %s" file reason