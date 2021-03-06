﻿using System;
using Prelude.Gameplay.DifficultyRating;
using Interlude.Graphics;

namespace Interlude.Interface.Widgets.Toolbar
{
    class ProfileInfoPanel : Widget
    {
        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            float x = Math.Max(250, Math.Min(800, SpriteBatch.Font1.MeasureText(Game.Options.Profile.Name, 35))) + 90;
            SpriteBatch.Font1.DrawTextToFill(Game.Options.Profile.Name, new Rect(bounds.Left, bounds.Bottom - 82, bounds.Left + x - 90, bounds.Bottom - 20), Game.Options.Theme.MenuFont, true, Game.Screens.DarkColor);
            SpriteBatch.Font1.DrawJustifiedText(Utils.RoundNumber(Game.Options.Profile.Stats.PhysicalMean[(int)Game.Options.Profile.DefaultKeymode]), 30f, bounds.Left + x, bounds.Bottom - 80, Game.Options.Theme.MenuFont, true, CalcUtils.PhysicalColor(Game.Options.Profile.Stats.PhysicalMean[(int)Game.Options.Profile.DefaultKeymode]));
            SpriteBatch.Font1.DrawJustifiedText(Utils.RoundNumber(Game.Options.Profile.Stats.TechnicalMean[(int)Game.Options.Profile.DefaultKeymode]), 30f, bounds.Left + x, bounds.Bottom - 45, Game.Options.Theme.MenuFont, true, CalcUtils.PhysicalColor(Game.Options.Profile.Stats.TechnicalMean[(int)Game.Options.Profile.DefaultKeymode]));
            SpriteBatch.Font2.DrawTextToFill("Click here to change profile...", new Rect(bounds.Left + 10, bounds.Bottom - 30, bounds.Left + x, bounds.Bottom), Game.Options.Theme.MenuFont, true, Game.Screens.DarkColor);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            float x = Math.Max(250, Math.Min(800, SpriteBatch.Font1.MeasureText(Game.Options.Profile.Name, 35))) + 90;
            if (ScreenUtils.CheckButtonClick(new Rect(bounds.Left + x - 90, bounds.Bottom - 80, bounds.Left + x, bounds.Bottom - 40)))
            {
                Game.Screens.AddDialog(new Dialogs.TopScoreDialog(false));
            }
            else if (ScreenUtils.CheckButtonClick(new Rect(bounds.Left + x - 90, bounds.Bottom - 40, bounds.Left + x, bounds.Bottom)))
            {
                Game.Screens.AddDialog(new Dialogs.TopScoreDialog(true));
            }
            else if (ScreenUtils.CheckButtonClick(new Rect(bounds.Left, bounds.Bottom - 80, bounds.Left + x - 90, bounds.Bottom)))
            {
                Game.Screens.AddDialog(new Dialogs.ProfileDialog((s) => { }));
            }
        }
    }
}
