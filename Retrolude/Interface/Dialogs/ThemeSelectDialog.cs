﻿using System;
using Interlude.Graphics;
using Interlude.Interface.Widgets;

namespace Interlude.Interface.Dialogs
{
    public class ThemeSelectDialog : FadeDialog
    {
        FlowContainer Selected, Available;

        class ThemeItem : Widget
        {
            public string theme;
            ThemeSelectDialog dialog;

            internal ThemeItem(string theme, ThemeSelectDialog d)
            {
                this.theme = theme;
                dialog = d;
                Reposition(0, 0, 0, 0, 0, 1, 50, 0);
            }

            public override void Update(Rect bounds)
            {
                base.Update(bounds);
                bounds = GetBounds(bounds);
                if (ScreenUtils.CheckButtonClick(bounds))
                {
                    if (Parent == dialog.Selected)
                    {
                        dialog.Selected.RemoveChild(this);
                        dialog.Available.AddChild(this);
                        Game.Options.Profile.SelectedThemes.Remove(theme);
                    }
                    else
                    {
                        dialog.Available.RemoveChild(this);
                        dialog.Selected.AddChild(this);
                        Game.Options.Profile.SelectedThemes.Add(theme);
                    }
                }
            }

            public override void Draw(Rect bounds)
            {
                base.Draw(bounds);
                bounds = GetBounds(bounds);
                SpriteBatch.DrawRect(bounds, Game.Screens.BaseColor);
                SpriteBatch.Font1.DrawCentredTextToFill(theme, bounds, System.Drawing.Color.White);
            }
        }

        public ThemeSelectDialog(Action<string> action) : base((s) => { Game.Options.Themes.Unload(); Game.Options.Themes.Load(); action(s); })
        {
            Game.Options.Themes.DetectAvailableThemes();
            AddChild(new SimpleButton("Done", () => { Game.Options.Themes.Unload(); Game.Options.Themes.Load(); Close(""); }, () => false, () => Game.Options.General.Hotkeys.Select).Reposition(0, 0.4f, -150, 1, 0, 0.6f, -100, 1));
            AddChild((Selected = new FlowContainer()).Reposition(50, 0.5f, 200, 0, -200, 1, -200, 1));
            AddChild((Available = new FlowContainer()).Reposition(200, 0, 200, 0, -50, 0.5f, -200, 1));
            Selected.AddChild(new TextBox("(fallback)", TextAnchor.CENTER, 0, true, Game.Options.Theme.MenuFont, System.Drawing.Color.Black).Reposition(0, 0, 0, 0, 0, 1, 50, 0));

            foreach (string t in Game.Options.Themes.AvailableThemes)
            {
                if (Game.Options.Profile.SelectedThemes.Contains(t))
                {
                    Selected.AddChild(new ThemeItem(t, this));
                }
                else
                {
                    Available.AddChild(new ThemeItem(t, this));
                }
            }
        }
    }
}
