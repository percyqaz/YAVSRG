namespace Prelude.Data.Library

open Prelude
open Prelude.Mods
open Prelude.Gameplay.Rulesets
open Prelude.Data.User
open Prelude.Data.Library

type SuggestionContext =
    {
        BaseChart: ChartMeta * Rate
        Mods: ModState
        Filter: FilteredSearch
        MinimumRate: Rate
        MaximumRate: Rate
        OnlyNewCharts: bool
        RulesetId: string
        Ruleset: Ruleset
        Library: Library
        UserDatabase: UserDatabase
    }
    member this.LibraryViewContext: LibraryViewContext =
        {
            Rate = let (_, rate) = this.BaseChart in rate
            RulesetId = this.RulesetId
            Ruleset = this.Ruleset
            Library = this.Library
            UserDatabase = this.UserDatabase
        }