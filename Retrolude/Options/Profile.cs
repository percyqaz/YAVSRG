﻿using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using OpenTK.Input;
using Newtonsoft.Json;
using Prelude.Utilities;
using Prelude.Gameplay.ScoreMetrics;
using static Prelude.Gameplay.DifficultyRating.KeyLayout;
using static Prelude.Gameplay.ScoreMetrics.ScoreSystem;
using Interlude.IO;
using Interlude.Gameplay;

namespace Interlude.Options
{
    public class Profile
    {
        [JsonIgnore]
        public string ProfilePath = "Default.json";
        [JsonIgnore]
        public double Rate = 1.0f;
        [JsonIgnore]
        public Keymode DefaultKeymode
        {
            get { return KeymodePreference ? PreferredKeymode : ToKeymode(Game.CurrentChart != null ? Game.CurrentChart.Keys : 4); }
        }

        //todo: creating this was a distaster. refactor by destroying this enum and using flat numbers again
        public enum Keymode
        {
            Key3 = 0,
            Key4 = 1,
            Key5 = 2,
            Key6 = 3,
            Key7 = 4,
            Key8 = 5,
            Key9 = 6,
            Key10 = 7
        }

        public static Keymode ToKeymode(int keys)
        {
            return (Keymode)(keys - 3);
        }

        public string Name = "Default Profile";
        public string UUID = Guid.NewGuid().ToString();
        public ProfileStats Stats = new ProfileStats();

        public float ScrollSpeed = 2.05f;
        public int HitPosition = 0;
        public bool HitLighting = false;
        public bool Upscroll = false;
        public float ScreenCoverUp = 0f;
        public float ScreenCoverDown = 0f;
        public float ScreenCoverFadeLength = 200;
        public float PerspectiveTilt = 0f;
        public float BackgroundDim = 0.5f;
        public string NoteSkin = "default";
        public ColorScheme ColorStyle = new ColorScheme(Colorizer.ColorStyle.Column);

        public Keymode PreferredKeymode = Keymode.Key4;
        public bool KeymodePreference = false;
        public List<string> SelectedThemes = new List<string>();
        public string ChartSortMode = "Title";
        public string ChartGroupMode = "Pack";
        public string ChartColorMode = "Nothing";

        public List<ScoreSystemData> ScoreSystems = new List<ScoreSystemData>();
        public int SelectedScoreSystem;
        //todo: choice of life systems goes here
        //todo: move to theme since number of ranks available is dependent on theme
        public float[] GradeThresholds = new float[] { 99, 98, 97, 96, 95, 94, 93, 92, 91, 90 };
        public float Pacemaker = 95f;
        public ScoreSavingPreference ScoreSavingPreference = ScoreSavingPreference.PASS;
        public HPFailType HPFailType = HPFailType.INSTANT; //todo: put back to NOFAIL

        //these are default binds
        public KeyBind[][] KeyBinds = new KeyBind[][]
        {
            new KeyBind[] { new KeyBind(Key.Left), new KeyBind(Key.Down), new KeyBind(Key.Right) },
            new KeyBind[] { new KeyBind(Key.Z), new KeyBind(Key.X), new KeyBind(Key.Period), new KeyBind(Key.Slash) },
            new KeyBind[] { new KeyBind(Key.Z), new KeyBind(Key.X), new KeyBind(Key.Space), new KeyBind(Key.Period), new KeyBind(Key.Slash) },
            new KeyBind[] { new KeyBind(Key.Z), new KeyBind(Key.X), new KeyBind(Key.C), new KeyBind(Key.Comma), new KeyBind(Key.Period), new KeyBind(Key.Slash) },
            new KeyBind[] { new KeyBind(Key.Z), new KeyBind(Key.X), new KeyBind(Key.C), new KeyBind(Key.Space), new KeyBind(Key.Comma), new KeyBind(Key.Period), new KeyBind(Key.Slash) },
            new KeyBind[] { new KeyBind(Key.Z), new KeyBind(Key.X), new KeyBind(Key.C), new KeyBind(Key.V), new KeyBind(Key.Comma), new KeyBind(Key.Period), new KeyBind(Key.Slash), new KeyBind(Key.RShift) },
            new KeyBind[] { new KeyBind(Key.Z), new KeyBind(Key.X), new KeyBind(Key.C), new KeyBind(Key.V), new KeyBind(Key.Space), new KeyBind(Key.Comma), new KeyBind(Key.Period), new KeyBind(Key.Slash), new KeyBind(Key.RShift) },
            new KeyBind[] { new KeyBind(Key.CapsLock), new KeyBind(Key.Q), new KeyBind(Key.W), new KeyBind(Key.E), new KeyBind(Key.V), new KeyBind(Key.Space), new KeyBind(Key.K), new KeyBind(Key.L), new KeyBind(Key.Semicolon), new KeyBind(Key.Quote) }
        };

        public Layout[] Playstyles = new Layout[] //default playstyles
        {
            Layout.OneHand, Layout.Spread, Layout.LeftOne, Layout.Spread, Layout.LeftOne, Layout.Spread, Layout.LeftOne, Layout.Spread
        };

        public ScoreSystem GetScoreSystem(int Index)
        {
            if (Index >= ScoreSystems.Count)
            {
                Index = 0;
            }
            if (ScoreSystems.Count == 0)
            {
                ScoreSystems.Add(new ScoreSystemData(ScoreType.Default, new DataGroup()));
            }
            var ss = ScoreSystems[Index];
            return ss.Instantiate();
        }

        public void Rename(string name)
        {
            Name = name;
            if (ProfilePath == "Default.json") ProfilePath = new Regex("[^a-zA-Z0-9_-]").Replace(name, "") + ".json";
        }
    }
}
