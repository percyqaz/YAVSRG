namespace Prelude.Data.Library

open Prelude
open Prelude.Gameplay.Rulesets
open Prelude.Data.User
open Prelude.Data.Library

type LibraryViewContext =
    {
        Rate: Rate
        RulesetId: string
        Ruleset: Ruleset
        Library: Library
        UserDatabase: UserDatabase
    }