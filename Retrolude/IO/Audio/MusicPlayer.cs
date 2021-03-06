﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Diagnostics;
using ManagedBass;

namespace Interlude.IO.Audio
{
    public class MusicPlayer
    {
        private static readonly int TIME_BEFORE_SONG = 3000;
        private bool USE_TIMER => Game.Options.General.AudioFix || Game.Screens.Current is Interface.Screens.ScreenEditor; //link this to a user setting. set to true if you want audio code to use manual timing instead of querying audio file position
        private readonly bool PREVENT_PITCH_CHANGE = false; //link this to a user setting. set to true if you want the pitch of the music to be the same on different rates

        private Track nowplaying;
        private Stopwatch timer = new Stopwatch();
        private double timer_begin;
        private double Rate;

        //waveform stuff
        private readonly float[] fft = new float[1024];
        public readonly float[] WaveForm = new float[256];
        public float Level { get; private set; }

        public bool IsPaused { get; private set; }
        public bool Using_Timer { get; private set; }
        public float LocalOffset = 0;
        public Action OnPlaybackFinish;

        public void SetVolume(float volume) //sets volume of audio output
        {
            Bass.GlobalStreamVolume = (int)(volume * 10000);
        }

        public void SetRate(double rate) //sets playback rate (needs to be done every time song is switched)
        {
            if (PREVENT_PITCH_CHANGE) Bass.ChannelSetAttribute(nowplaying, ChannelAttribute.Pitch, -Math.Log(rate, 2) * 12);
            Bass.ChannelSetAttribute(nowplaying, ChannelAttribute.Frequency, nowplaying.Frequency * rate);
            Rate = rate;
        }

        protected double AudioOffset { get { return Game.Options.General.UniversalAudioOffset * Rate + LocalOffset; } } //local offset doesn't scale with rate. universal does

        public double Duration //gets duration of the song (NOT THE CHART) - used for progress bar through song not chart
        {
            get
            {
                return nowplaying.Duration;
            }
        }

        public bool Playing //gets whether audio player is playing
        {
            get
            {
                return Now()+AudioOffset < nowplaying?.Duration;
            }
        }

        public void PlayLeadIn() //plays the song but starting with a 3 second lead in
        {
            Play((long)(-TIME_BEFORE_SONG / Rate));
        }

        public void Play(long start) //plays the song from a given point (in ms) into the song.
        {
            Stop();
            Seek(start);
            Play();
        }

        public void Play() //plays the song from the beginning (0ms) OR unpauses the song if paused
        {
            IsPaused = false;
            if (!Using_Timer && Now() + AudioOffset >= 0)
            {
                Bass.ChannelPlay(nowplaying);
            }
            timer.Start();
        }

        public void Stop() //stops song playback (it resets to the start)
        {
            IsPaused = true;
            Bass.ChannelStop(nowplaying);
            timer.Stop();
            timer.Reset();
            Seek(0);
        }

        public void Pause() //pauses the song. Play() will resume
        {
            IsPaused = true;
            Bass.ChannelPause(nowplaying);
            timer.Stop();
        }

        public double Now() //gets position (in ms) we are into the song. This value is directly used in gameplay to sync notes to audio
            //audio offset is accounted for here to sync things. the number can be negative when leading into the audio file and will continue running once the audio file is over
            //some files like Go for it (golgo) and 1hr54 js challenge have notes continuing after audio ends so this is necessary
        {
            if (nowplaying == null) return 0;
            if (Using_Timer || USE_TIMER) return (long)(timer.ElapsedMilliseconds * Rate + timer_begin) - AudioOffset;
            return Bass.ChannelBytes2Seconds(nowplaying, Bass.ChannelGetPosition(nowplaying)) * 1000 - AudioOffset;
        }

        public float NowPercentage() //gets position through the song (NOT THE CHART) as a percentage (0-1)
        {
            if (nowplaying == null) return 0;
            return (float)(Now() / Duration);
        }

        public void Seek(double position) //jumps to a position in a song (doesn't do anything to pause, play or stop)
        {
            var b = timer.IsRunning;
            timer.Reset(); if (b) timer.Start();
            timer_begin = position;
            if (position < 0 || position > Duration)
            {
                Using_Timer = true;
            }
            else
            {
                Using_Timer = false;
                Bass.ChannelSetPosition(nowplaying, Bass.ChannelSeconds2Bytes(nowplaying, position / 1000));
            }
        }

        public void UpdateWaveform() //updates the waveform. done every update frame
        {
            //algorithm adapted from here
            //https://www.codeproject.com/Articles/797537/Making-an-Audio-Spectrum-analyzer-with-Bass-dll-Cs
            //thanks very much it was significantly better than my old algorithm
            if (IsPaused || !Playing)
            {
                for (int x = 0; x < 256; x++)
                {
                    WaveForm[x] = WaveForm[x] * 0.9f;
                }
                Level *= 0.9f;
                return;
            }
            Bass.ChannelGetData(nowplaying, fft, (int)DataFlags.FFT2048); //pull new raw waveform data
            int b0 = 0;
            int y;
            for (int x = 0; x < 256; x++) //just some maths u dont need to understand
            {
                float peak = 0;
                int b1 = (int)Math.Pow(2, x * 10.0 / 255);
                if (b1 > 1023) b1 = 1023;
                if (b1 <= b0) b1 = b0 + 1;
                for (; b0 < b1; b0++)
                {
                    if (peak < fft[1 + b0]) peak = fft[1 + b0];
                }
                y = (int)(Math.Sqrt(peak) * 3 * 255 - 4);
                if (y > 255) y = 255;
                if (y < 0) y = 0;
                WaveForm[x] = WaveForm[x] * 0.9f + y * 0.1f; //causes smooth movement of waveform rather than being EXACTLY the amplitudes at this very moment
            }
            Level = Level * 0.9f + (Bass.ChannelGetLevelRight(nowplaying) + Bass.ChannelGetLevelLeft(nowplaying)) * 0.0000002f;
        }

        public void Update() //updates the music player every update frame
        {
            float[] temp = new float[256];
            UpdateWaveform();
            if (Using_Timer && Playing && Now() + AudioOffset >= 0) //if leadin timer is complete
            {
                Seek(Now() + AudioOffset); //synchronise to the audio and start playing (sounds smooth and avoids the bug where notes teleport to receptors)
                Bass.ChannelPlay(nowplaying);
            }
            if (!Playing || (!IsPaused && Playing && Bass.ChannelIsActive(nowplaying) == PlaybackState.Stopped)) //if the song is over run the callback which can be assigned from elsewhere in the game
            {
                if (OnPlaybackFinish != null)
                {
                    OnPlaybackFinish(); //this is can restart the song from the beginning, select another song at random, etc
                }
                else if (!Using_Timer) { Using_Timer = true; } //if not assigned the timer will just keep running (used in gameplay)
            }
        }

        public void ChangeTrack(string path) //switches to a different audio file given an absolute file path (supports ogg, wav, mp3 and i think some others)
        {
            var t = new Track(path);
            nowplaying?.Dispose(); //destroy old track / free resources
            nowplaying = t;
        }

        public void PlaySFX(string name, float pitch = 1f, float volume = 1f)
        {
            int i = Game.Options.Themes.GetSound(name);
            int s = Bass.SampleGetChannel(i);
            Bass.ChannelSetAttribute(s, ChannelAttribute.Volume, Game.Options.General.AudioVolume * volume);
            Bass.ChannelSetAttribute(s, ChannelAttribute.Frequency, Bass.ChannelGetAttribute(s, ChannelAttribute.Frequency) * pitch);
            Bass.ChannelPlay(s);
        }
    }
}
