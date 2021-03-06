﻿using System;
using Interlude.Gameplay;
using Interlude.Interface.Animations;
using Interlude.Graphics;

namespace Interlude.Interface.Widgets.Gameplay
{
    public class ComboDisplay : GameplayWidget
    {
        AnimationSlider size;
        int baseSize, bumpAmount, cbAmount, comboCap;
        float scaleWithCombo;

        public ComboDisplay(ScoreTracker scoreTracker, Options.WidgetPosition pos) : base(scoreTracker, pos)
        {
            baseSize = pos.Extra.GetValue("baseSize", 40);
            bumpAmount = pos.Extra.GetValue("hitBumpAmount", 5);
            cbAmount = pos.Extra.GetValue("missBumpAmount", 40);
            comboCap = pos.Extra.GetValue("comboCap", 1000);
            scaleWithCombo = pos.Extra.GetValue("scaleWithCombo", 0.02f);

            size = new AnimationSlider(baseSize);
            scoreTracker.OnHit += (x,y,z) =>
            {
                size.Val = baseSize + bumpAmount;
                if (scoreTracker.Scoring.Combo == 0)
                {
                    size.Val = baseSize + cbAmount;
                }
            };
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            float s = Math.Min(comboCap, scoreTracker.Scoring.Combo) * scaleWithCombo + size;
            SpriteBatch.Font1.DrawCentredText(scoreTracker.Scoring.Combo.ToString(), s, bounds.CenterX, bounds.Top - s / 2, scoreTracker.WidgetColor);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            size.Update();
        }
    }
}
