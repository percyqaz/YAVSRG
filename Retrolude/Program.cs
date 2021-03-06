﻿using System;
using System.Threading;
using System.Diagnostics;
using Prelude.Utilities;
using Interlude.Utilities;
using Interlude.IO;

namespace Interlude
{
    class Program
    {
        static void Main(string[] args)
        {
            Mutex m = new Mutex(true, "Interlude");
            if (m.WaitOne(TimeSpan.Zero, true))
            {
                Process.GetCurrentProcess().PriorityClass = ProcessPriorityClass.High;
                PipeHandler.Open();
                Logging.Log("Launching " + Game.Version + ", the date/time is " + DateTime.Now.ToString(), "");
                Game g = null;
                try
                {
                    Options.SettingsManager.Init(); //init options i.e load profiles
                    g = new Game();
                    Logging.Debug("Game initiated successfully", locale: false);
                }
                catch (Exception e)
                {
                    CrashWindow.DisplayCrashWindow(e.ToString());
                    Logging.Log("Game failed to launch", e.ToString(), Logging.LogType.Critical);
                }
                if (g != null)
                {
                    try
                    {
                        g.Run(120.0); //run the game
                    }
                    catch (Exception e)
                    {
                        g.Exit(); //if it crashes close it and give a neat crash log
                        CrashWindow.DisplayCrashWindow(e.ToString());
                        Logging.Log("Game crashed (that's bad)", e.ToString(), Logging.LogType.Critical);
                    }
                    finally
                    {
                        g.Exit();
                        g.Dispose(); //clean up resources. i don't know if there's anything left to clean up but it's here i guess
                    }
                }
                Logging.Close();
                PipeHandler.Close();
                m.ReleaseMutex();
            }
            else
            {
                //if (args.Length > 0)
                //{
                //    PipeHandler.SendData("open", args[0]); disabled for now cause it's a liability
                //}
                //else
                //{
                    PipeHandler.SendData("show", "");
                //}
            }
        }
    }
}
