﻿using System;
using System.Drawing;
using Interlude.Graphics;

namespace Interlude.Interface.Widgets
{
    public class TextBox : Widget
    {
        Func<string> text;
        TextAnchor style;
        bool font;
        float size;
        Func<Color> color;
        Func<Color> dropColor;

        public TextBox(string text, TextAnchor position, float textSize, bool altFont, Color c, Color d = default(Color)) : this(() => (text), position, textSize, altFont, () => (c), () => (d)) { }

        public TextBox(Func<string> text, TextAnchor position, float textSize, bool altFont, Func<Color> c, Func<Color> d)
        {
            this.text = text;
            style = position;
            font = altFont;
            size = textSize;
            color = c;
            dropColor = d;
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            Color d = dropColor();
            if (size == 0)
            {
                (font ? SpriteBatch.Font1 : SpriteBatch.Font2).DrawDynamicTextToFill(text(), bounds, color(), style, d.A > 0, d);
            }
            else
            {
                (font ? SpriteBatch.Font1 : SpriteBatch.Font2).DrawDynamicText(text(), bounds, color(), style, size, d.A > 0, d);
            }
        }
    }
}
