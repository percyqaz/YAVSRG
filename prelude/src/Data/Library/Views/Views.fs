namespace Prelude.Data.Library

open System
open System.Collections.Generic
open Prelude.Backbeat
open Prelude.Data.Library

type Group =
    {
        Charts: (ChartMeta * LibraryContext) array
        Context: LibraryGroupContext
    }

type private GroupWithSorting =
    {
        Charts: ResizeArray<ChartMeta * LibraryContext * SortingTag>
        Context: LibraryGroupContext
    }
    member this.ToGroup (reverse_sorting: bool) : Group =
        {
            Charts =
                this.Charts
                |> Seq.sortBy (fun (_, _, key) -> key)
                |> if reverse_sorting then Seq.rev else id
                |> Seq.map (fun (chart_meta, ctx, _) -> (chart_meta, ctx))
                |> Array.ofSeq
            Context = this.Context
        }

type SortedGroups = (string * Group) seq

type GroupFunc = ChartMeta * LibraryViewContext -> int * string

type LibraryView =
    | Groups of GroupFunc
    | Packs
    | Collections
    | Levels
    | Category
    | Suggestions of SuggestionContext

module LibraryView =

    let USER_SELECTABLE_VIEWS: IDictionary<string, LibraryView> =
        dict
            [
                "none", Groups <| fun (c, _) -> 0, "No grouping"
                "pack", Packs
                "collection", Collections
                "level", Levels
                "category", Category
                "difficulty", Groups format_difficulty
                "date_played", Groups format_date_last_played
                "date_installed", Groups format_date_added
                "grade", Groups grade_achieved
                "lamp", Groups lamp_achieved
                "title", Groups <| fun (c, _) -> 0, first_character c.Title
                "artist", Groups <| fun (c, _) -> 0, first_character c.Artist
                "creator", Groups <| fun (c, _) -> 0, first_character c.Creator
                "keymode", Groups <| fun (c, _) -> c.Keys, c.Keys.ToString() + "K"
            ]

    // todo: sort 3, 3.5, 4 in correct order
    let private group_name_to_smart_sort_list (name: string) : string list =
        name
            .ToLowerInvariant()
            .Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map (fun word -> if String.forall Char.IsAsciiDigit word then word.PadLeft(4, '0') else word)
        |> List.ofSeq

    let private get_collection_groups
        (filter_by: FilteredSearch)
        (reverse_groups: bool)
        (sort_by: SortMethod)
        (reverse_sorting: bool)
        (ctx: LibraryViewContext)
        : SortedGroups =

        let groups = new Dictionary<string, Group>()

        for name in ctx.Library.Collections.Folders.Keys do
            let collection = ctx.Library.Collections.Folders.[name]

            collection.Charts
            |> Seq.choose (fun entry ->
                match ChartDatabase.get_meta entry.Hash ctx.Library.Charts with
                | Some chart_meta -> Some(chart_meta, LibraryContext.Folder name)
                | None -> None
            )
            |> filter_by.Apply
            |> Seq.sortBy (fun (chart_meta, _) -> sort_by (chart_meta, ctx))
            |> if reverse_sorting then Seq.rev else id
            |> Array.ofSeq
            |> fun x ->
                if x.Length > 0 then
                    groups.Add(
                        name,
                        {
                            Charts = x
                            Context = LibraryGroupContext.Folder name
                        }
                    )

        for name in ctx.Library.Collections.Playlists.Keys do
            let playlist = ctx.Library.Collections.Playlists.[name]

            playlist.Charts
            |> Seq.indexed
            |> Seq.choose (fun (i, (entry, info)) ->
                match ChartDatabase.get_meta entry.Hash ctx.Library.Charts with
                | Some chart_meta -> Some(chart_meta, LibraryContext.Playlist(i, name, info))
                | None -> None
            )
            |> filter_by.Apply
            |> Array.ofSeq
            |> fun x ->
                if x.Length > 0 then
                    groups.Add(
                        name,
                        {
                            Charts = x
                            Context = LibraryGroupContext.Playlist name
                        }
                    )

        let liked_songs : Group option =
            ctx.Library.Collections.EnumerateLikes
            |> Seq.choose (fun chart_id -> ChartDatabase.get_meta chart_id ctx.Library.Charts)
            |> filter_by.Apply
            |> Seq.sortBy (fun chart_meta -> sort_by (chart_meta, ctx))
            |> if reverse_sorting then Seq.rev else id
            |> Seq.map (fun chart_meta -> (chart_meta, LibraryContext.Likes))
            |> Array.ofSeq
            |> fun x ->
                if x.Length > 0 then
                    Some {
                        Charts = x
                        Context = LibraryGroupContext.Likes
                    }
                else None

        seq {
            match liked_songs with
            | Some likes -> yield "Liked songs", likes
            | None -> ()

            yield!
                groups
                |> Seq.sortBy (fun kvp -> group_name_to_smart_sort_list kvp.Key)
                |> if reverse_groups then Seq.rev else id
                |> Seq.map (|KeyValue|)
        }

    let private get_table_groups
        (filter_by: FilteredSearch)
        (reverse_groups: bool)
        (sort_by: SortMethod)
        (reverse_sorting: bool)
        (table: Table)
        (ctx: LibraryViewContext)
        : SortedGroups =
        let groups = new Dictionary<int * string, Group>()

        for level, charts in table.Charts |> Seq.groupBy (fun x -> x.Level) do
            charts
            |> Seq.choose (fun (c: TableChart) ->
                match ChartDatabase.get_meta c.Hash ctx.Library.Charts with
                | Some chart_meta -> Some(chart_meta, LibraryContext.Table level)
                | None -> None
            )
            |> filter_by.Apply
            |> Seq.sortBy (fun (chart_meta, _) -> sort_by (chart_meta, ctx))
            |> if reverse_sorting then Seq.rev else id
            |> Array.ofSeq
            |> fun x ->
                if x.Length > 0 then
                    groups.Add(
                        (level,
                         table.Info.LevelDisplayNames.TryFind level
                         |> Option.defaultValue (level.ToString())),
                        {
                            Charts = x
                            Context = LibraryGroupContext.Table level
                        }
                    )

        groups
        |> Seq.sortBy (_.Key >> (fun (index, group_name) -> (index, group_name.ToLowerInvariant())))
        |> if reverse_groups then Seq.rev else id
        |> Seq.map (fun kvp -> snd kvp.Key, kvp.Value)

    let private get_normal_groups
        (filter_by: FilteredSearch)
        (group_by: GroupFunc)
        (reverse_groups: bool)
        (sort_by: SortMethod)
        (reverse_sorting: bool)
        (ctx: LibraryViewContext)
        : SortedGroups =

        let found_groups = new Dictionary<int * string, GroupWithSorting>()

        for chart_meta in filter_by.Apply ctx.Library.Charts.Cache.Values do
            let group_key = group_by (chart_meta, ctx)

            if found_groups.ContainsKey group_key |> not then
                found_groups.Add(
                    group_key,
                    {
                        Charts = ResizeArray<ChartMeta * LibraryContext * SortingTag>()
                        Context = LibraryGroupContext.None
                    }
                )

            found_groups.[group_key].Charts.Add(chart_meta, LibraryContext.None, sort_by (chart_meta, ctx))

        found_groups
        |> Seq.sortBy (_.Key >> (fun (index, group_name) -> (index, group_name.ToLowerInvariant())))
        |> if reverse_groups then Seq.rev else id
        |> Seq.map (fun kvp -> snd kvp.Key, kvp.Value.ToGroup reverse_sorting)

    let private get_packs
        (filter_by: FilteredSearch)
        (reverse_groups: bool)
        (sort_by: SortMethod)
        (reverse_sorting: bool)
        (ctx: LibraryViewContext)
        : SortedGroups =

        let found_groups = new Dictionary<string, GroupWithSorting>()

        for chart_meta in filter_by.Apply ctx.Library.Charts.Cache.Values do
            for pack in chart_meta.Packs do

                if found_groups.ContainsKey pack |> not then
                    found_groups.Add(
                        pack,
                        {
                            Charts = ResizeArray<ChartMeta * LibraryContext * SortingTag>()
                            Context = LibraryGroupContext.Pack pack
                        }
                    )

                found_groups.[pack].Charts.Add(chart_meta, LibraryContext.Pack pack, sort_by (chart_meta, ctx))

        found_groups
        |> Seq.sortBy (fun kvp -> group_name_to_smart_sort_list kvp.Key)
        |> if reverse_groups then Seq.rev else id
        |> Seq.map (fun kvp -> kvp.Key, kvp.Value.ToGroup reverse_sorting)

    let private get_categories
        (filter_by: FilteredSearch)
        (reverse_groups: bool)
        (sort_by: SortMethod)
        (reverse_sorting: bool)
        (ctx: LibraryViewContext)
        : SortedGroups =

        let groups = new Dictionary<Prelude.Calculator.Patterns.ChartTag, GroupWithSorting>()
        for chart_meta in filter_by.Apply ctx.Library.Charts.Cache.Values do
            for tag in chart_meta.Patterns.Tags do

                if groups.ContainsKey tag |> not then
                    groups.Add(
                        tag,
                        {
                            Charts = ResizeArray<ChartMeta * LibraryContext * SortingTag>()
                            Context = LibraryGroupContext.None
                        }
                    )

                groups.[tag].Charts.Add(chart_meta, LibraryContext.Category tag, sort_by (chart_meta, ctx))

        groups
        |> Seq.sortBy (fun kvp -> kvp.Key)
        |> if reverse_groups then Seq.rev else id
        |> Seq.map (fun kvp -> kvp.Key.ToString(), kvp.Value.ToGroup reverse_sorting)

    let get_groups
        (filter_by: FilteredSearch)
        (group_by: LibraryView)
        (reverse_groups: bool)
        (sort_by: SortMethod)
        (reverse_sorting: bool)
        (always_show_collections: bool)
        (table: Table option)
        (ctx: LibraryViewContext)
        : SortedGroups =

        let groups =
            match group_by with
            | Groups func -> get_normal_groups filter_by func reverse_groups sort_by reverse_sorting ctx

            | Packs -> get_packs filter_by reverse_groups sort_by reverse_sorting ctx

            | Collections -> get_collection_groups filter_by reverse_groups sort_by reverse_sorting ctx

            | Levels ->
                match table with
                | Some t -> get_table_groups filter_by reverse_groups sort_by reverse_sorting t ctx
                | None -> Seq.empty

            | Category -> get_categories filter_by reverse_groups sort_by reverse_sorting ctx

            | Suggestions suggestions_ctx ->

                [
                    "Suggestions for " + (fst suggestions_ctx.BaseChart).Title,
                    {
                        Charts =
                            Endless.Suggestion.get_core_suggestions suggestions_ctx
                            |> Seq.truncate 50
                            |> Array.ofSeq
                            |> Array.map (fun (chart_meta, rate) -> (chart_meta, LibraryContext.Suggestion rate, ("", chart_meta.Patterns.EstimatedDifficulty rate, 0.0f)))
                            |> ResizeArray
                        Context = LibraryGroupContext.None
                    }
                        .ToGroup reverse_sorting
                ]

        match group_by with
        | Groups _
        | Packs when always_show_collections ->
            let collections = get_collection_groups filter_by reverse_groups sort_by reverse_sorting ctx
            Seq.concat [collections; groups]
        | _ -> groups