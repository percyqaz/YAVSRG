﻿using System;
using System.Collections.Generic;
using System.Drawing;
using System.Globalization;
using Prelude.Utilities;
using Interlude.IO;
using Interlude.Graphics;

namespace Interlude.Interface.Widgets.Toolbar
{
    class ChatBox : Widget
    {
        class ChatChannel
        {
            public List<string> lines;
            public SimpleButton button;

            public ChatChannel(SimpleButton b)
            {
                lines = new List<string>();
                button = b;
            }
        }

        class EmojiPicker : Widget
        {
            bool expand;
            readonly Color[] colors = new Color[] { Color.Red, Color.Green, Color.Blue, Color.Yellow, Color.Aqua, Color.Purple, Color.Orange, Color.Indigo, Color.Lavender, Color.HotPink };

            public override void Draw(Rect bounds)
            {
                base.Draw(bounds);
                bounds = GetBounds(bounds);
                float spacing = 40f;
                if (expand)
                {
                    ScreenUtils.DrawFrame(bounds, Color.White);
                    for (int x = 0; x < 5; x++)
                    {
                        for (int y = 0; y < 4; y++)
                        {
                            SpriteBatch.Draw(new RenderTarget(Game.Options.Themes.GetTexture("emoji"), new Rect(bounds.Left + 10 + spacing * x, bounds.Top + 10 + spacing * y, bounds.Left + spacing + spacing * x, bounds.Top + spacing + spacing * y), Color.White, x + y * 5, 0));
                        }
                        for (int y = 4; y < 6; y++)
                        {
                            SpriteBatch.DrawRect(new Rect(bounds.Left + 10 + spacing * x, bounds.Top + 10 + spacing * y, bounds.Left + spacing + spacing * x, bounds.Top + spacing + spacing * y), colors[x + y * 5 - 20]);
                        }
                    }
                }
                else
                {
                    SpriteBatch.Draw(new RenderTarget(Game.Options.Themes.GetTexture("emoji"), new Rect(bounds.Right - spacing, bounds.Top + 10, bounds.Right - 10, bounds.Top + spacing), Color.White, 0, 0));
                }
            }

            public override void Update(Rect bounds)
            {
                base.Update(bounds);
                bounds = GetBounds(bounds);
                float spacing = 40f;
                if (expand)
                {
                    for (int x = 0; x < 5; x++)
                    {
                        for (int y = 0; y < 4; y++)
                        {
                            if (ScreenUtils.CheckButtonClick(new Rect(bounds.Left + 10 + spacing * x, bounds.Top + 10 + spacing * y, bounds.Left + spacing + spacing * x, bounds.Top + spacing + spacing * y)))
                            {
                                ((ChatBox)Parent).entryText += "{e:" + (x + y * 5).ToString() + "}";
                                if (!Input.KeyPress(OpenTK.Input.Key.ControlLeft, true))
                                {
                                    expand = false;
                                }
                            }
                        }
                        for (int y = 4; y < 6; y++)
                        {
                            if (ScreenUtils.CheckButtonClick(new Rect(bounds.Left + 10 + spacing * x, bounds.Top + 10 + spacing * y, bounds.Left + spacing + spacing * x, bounds.Top + spacing + spacing * y)))
                            {
                                Color c = colors[x + y * 5 - 20];
                                ((ChatBox)Parent).entryText += "{c:" + c.R.ToString("X2") + c.G.ToString("X2") + c.B.ToString("X2") + "}";
                                if (!Input.KeyPress(OpenTK.Input.Key.ControlLeft, true))
                                {
                                    expand = false;
                                }
                            }
                        }
                    }
                }
                else
                {
                    if (ScreenUtils.CheckButtonClick(new Rect(bounds.Right - spacing, bounds.Top + 10, bounds.Right - 10, bounds.Top + spacing)))
                    {
                        expand = true;
                    }
                }
            }
        }

        public bool Collapsed
        {
            get
            {
                return fade.Target == 0;
            }
        }

        Dictionary<string, ChatChannel> channels;
        string selectedChannel = "";
        string entryText = "";
        int newMessages = 0;
        FlowContainer channelSelector;
        Animations.AnimationSlider newMsgFade, fade;
        Widget emoji;

        public ChatBox()
        {
            channels = new Dictionary<string, ChatChannel>();
            channelSelector = new FlowContainer() { BackColor = () => Color.FromArgb(127, 0, 0, 0), UseBackground = false, VerticalFade = 0 };
            AddChild(channelSelector.Reposition(20, 0, 20, 0, -20, 1, 80, 0));
            AddChild(emoji = new EmojiPicker().Reposition(-240, 1, 30, 0, -30, 1, -30, 1));
            emoji.SetState(WidgetState.DISABLED);
            Animation.Add(newMsgFade = new Animations.AnimationSlider(0));
            Animation.Add(fade = new Animations.AnimationSlider(0));
            Logging.OnLog += (s, d, t) => AddLine("Log", "[" + t.ToString() + "] " + s + (d != "" ? " (See log.txt for details)" : ""), false);
        }

        public override void Draw(Rect bounds)
        {
            bounds = GetBounds(bounds);
            if (newMsgFade.Val > 0.01f)
            {
                int a = (int)(255 * newMsgFade.Val);
                var c = Color.FromArgb(0, 0, 0, 0);
                SpriteBatch.Draw(new RenderTarget(new Rect(bounds.Right - 1200, bounds.Bottom - 380, bounds.Right, bounds.Bottom - 80), c, c, Color.FromArgb(a, Color.Black), c));
                SpriteBatch.Font1.DrawJustifiedText("Press " + Game.Options.General.Hotkeys.Chat.ToString() + " to view " + newMessages.ToString() + " new message" + (newMessages == 1 ? "" : "s"), 30f, bounds.Right, bounds.Bottom - 130, Color.FromArgb(a, Game.Options.Theme.MenuFont));
            }
            if (Game.Tasks.Tasks.Count > 0)
            {
                var b = new Rect(bounds.Right - 500, bounds.Bottom - 60, bounds.Right - 250, bounds.Bottom - 20);
                SpriteBatch.DrawRect(b, Color.FromArgb(120, 0, 0, 0));
                SpriteBatch.Font1.DrawTextToFill(Game.Tasks.Tasks.Count.ToString() + " background task(s) running", b, Color.White);
            }
            if (fade > 0.01f)
            {
                using (FBO fbo = FBO.FromPool())
                {
                    SpriteBatch.DrawRect(bounds, Color.FromArgb(180, 0, 0, 0));
                    SpriteBatch.DrawRect(new Rect(bounds.Left + 20, bounds.Top + 100, bounds.Right - 20, bounds.Bottom - 20), Color.FromArgb(127, Color.Black));
                    DrawWidgets(bounds);
                    if (selectedChannel != "")
                    {
                        var l = channels[selectedChannel].lines;
                        int c = Math.Min(l.Count, (int)(bounds.Height / 25 - 6));
                        for (int i = 0; i < c; i++)
                        {
                            RenderText(l[i], bounds.Left + 40, bounds.Bottom - 90 - 25 * i);
                        }
                    }
                    RenderText("> " + entryText, bounds.Left + 40, bounds.Bottom - 60);

                    if (emoji.State == WidgetState.DISABLED)
                    {
                        lock (Game.Tasks.Tasks)
                        {
                            float y = bounds.Top + 110;
                            foreach (Utilities.TaskManager.NamedTask t in Game.Tasks.Tasks)
                            {
                                SpriteBatch.Font1.DrawJustifiedText(t.Name, 30f, bounds.Right - 30, y, Game.Options.Theme.MenuFont, true);
                                SpriteBatch.Font2.DrawJustifiedText(t.Progress + " // " + t.Status.ToString(), 20f, bounds.Right - 30, y + 40f, Game.Options.Theme.MenuFont);
                                y += 60f;
                            }
                        }
                    }

                    ScreenUtils.DrawFrame(new Rect(bounds.Left + 20, bounds.Top + 100, bounds.Right - 20, bounds.Bottom - 20), Game.Screens.HighlightColor);
                    fbo.Unbind();
                    SpriteBatch.Draw(new RenderTarget(fbo, ScreenUtils.Bounds, Color.FromArgb((int)(255 * fade), Color.White)));
                }
            }
        }

        public override void Update(Rect bounds)
        {
            if (!Collapsed)
            {
                base.Update(bounds);
            }
            else
            {
                Animation.Update();
            }
            bounds = GetBounds(bounds);
            if (Collapsed)
            {
                if (Game.Options.General.Hotkeys.Chat.Tapped() && ((Interface.Toolbar)Parent).State != WidgetState.DISABLED)
                {
                    Expand();
                    ((Interface.Toolbar)Parent).SetState(WidgetState.ACTIVE);
                }
            }
            else
            {
                if (Game.Options.General.Hotkeys.Chat.Tapped(true) || Game.Options.General.Hotkeys.Exit.Tapped(true))
                {
                    Collapse();
                }
                else if (entryText != "" && Input.KeyTap(OpenTK.Input.Key.Enter, true))
                {
                    SendMessage(selectedChannel, entryText);
                    entryText = "";
                }
                if (ScreenUtils.CheckButtonClick(bounds))
                {
                    Input.ChangeIM(new InputMethod((s) => { entryText = s; }, () => { return entryText; }, () => { }));
                }
            }
        }

        public void AddLine(string channel, string text, bool important)
        {
            if (!channels.ContainsKey(channel))
            {
                CreateChannel(channel);
            }
            var l = channels[channel];
            l.lines.Insert(0,text); //no limit to chat history
            //if i want one, put it here
            if (Collapsed && important)
            {
                newMessages += 1;
                newMsgFade.Target = 1;
            }
        }

        public void Collapse()
        {
            fade.Target = 0;
            Input.ChangeIM(null);
        }

        public void Expand()
        {
            fade.Target = 1;
            Input.ChangeIM(new InputMethod((s) => { entryText = s; }, () => { return entryText; }, () => { }));
            newMessages = 0;
            newMsgFade.Target = 0;
        }

        void RenderText(string text, float x, float y)
        {
            int index = 0;
            Color c = Color.White;
            for (int i = index; i < text.Length; i++)
            {
                if (text[i] == '{')
                {
                    x += SpriteBatch.Font2.DrawText(text.Substring(index, i - index), 20f, x, y, c);
                    index = i;
                }
                else if (text[i] == '}')
                {
                    string[] parse = text.Substring(index + 1, i - index - 1).Split(':');
                    bool valid = false;
                    if (parse.Length > 1)
                    {
                        if (parse[0] == "c")
                        {
                            int argb = -1;
                            int.TryParse(parse[1], NumberStyles.HexNumber, CultureInfo.InvariantCulture, out argb);
                            c = Color.FromArgb(255,Color.FromArgb(argb));
                            valid = true;
                        }
                        else if (parse[0] == "e")
                        {
                            int id;
                            int.TryParse(parse[1], out id);
                            SpriteBatch.Draw(new RenderTarget(Game.Options.Themes.GetTexture("emoji"), new Rect(x, y, x + 35, y + 35), c, id, 0));
                            x += 35;
                            valid = true;
                        }
                    }
                    if (!valid)
                    {
                        x += SpriteBatch.Font2.DrawText(text.Substring(index, i - index), 20f, x, y, c);
                    }
                    index = i + (valid ? 1 : 0);
                }
            }
            x += SpriteBatch.Font2.DrawText(text.Substring(index), 20f, x, y, c);
        }

        void SendMessage(string channel, string msg)
        {
            if (channel == "Lobby" && Game.Online.Connected)
            {
                Game.Online.SendMessage(msg);
            }
        }

        void CreateChannel(string channel)
        {
            channels.Add(channel, new ChatChannel(new SimpleButton(channel, () => { selectedChannel = channel; emoji.SetState(channel == "Log" ? WidgetState.DISABLED : WidgetState.NORMAL); }, () => { return selectedChannel == channel; }, null)));
            channelSelector.AddChild(channels[channel].button.Reposition(0, 0, 0, 0, 180, 0, 40, 0));
            if (selectedChannel == "") { selectedChannel = channel; }
        }

        void RemoveChannel(string channel)
        {
            channelSelector.RemoveChild(channels[channel].button);
            channels.Remove(channel);
            if (channel == selectedChannel) { selectedChannel = ""; }
        }
    }
}
