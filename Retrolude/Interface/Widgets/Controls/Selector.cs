﻿using System;
using System.Drawing;
using OpenTK.Input;
using Prelude.Utilities;
using Interlude.Graphics;
using Interlude.IO;

namespace Interlude.Interface.Widgets
{
    //Allows a user to cycle through/select from a list of strings and store the index of the selected item in a variables
    public class Selector : Widget
    {
        string[] Options;
        string Label;
        SetterGetter<int> Value;

        public Selector(string name, string[] options, SetterGetter<int> value) : base()
        {
            Label = name != "" ? name + ": " : name;
            Options = options;
            Value = value;
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            SpriteBatch.DrawRect(bounds, Color.FromArgb(120, Game.Screens.DarkColor));
            SpriteBatch.Font1.DrawCentredText(Label+Options[Value], 30, bounds.CenterX, bounds.Top, Game.Options.Theme.MenuFont, true, Game.Screens.DarkColor);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            bounds = GetBounds(bounds);
            if (ScreenUtils.MouseOver(bounds))
            {
                if (Input.MouseClick(MouseButton.Left) || Game.Options.General.Hotkeys.Next.Tapped())
                {
                    Value.Set(Utils.Modulus(Value + 1, Options.Length));
                }
                else if (Input.MouseClick(MouseButton.Right) || Game.Options.General.Hotkeys.Previous.Tapped())
                {
                    Value.Set(Utils.Modulus(Value - 1, Options.Length));
                }
            }
        }

        public static Selector FromEnum<T>(string name, object target, string propertyName) where T : Enum
        {
            var s = new SetterGetter<int>(target, propertyName);
            return new Selector(name, typeof(T).GetEnumNames(), s);
        }
    }
}
