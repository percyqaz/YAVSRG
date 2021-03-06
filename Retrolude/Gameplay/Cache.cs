﻿using System.Collections.Generic;
using Prelude.Gameplay.Charts.YAVSRG;
using System.IO;

namespace Interlude.Gameplay
{
    public class Cache
    {
        [Newtonsoft.Json.JsonIgnore]
        static readonly string CacheVersion = "1.1";

        static string GetCachePath()
        {
            return Path.Combine(Game.WorkingDirectory, "Data", "Cache.json");
        }

        public static Cache LoadCache()
        {
            string path = GetCachePath();
            if (File.Exists(path))
            {
                Cache c = Utils.LoadObject<Cache>(path);
                if (c.Version == CacheVersion)
                {
                    return c;
                }
            }
            return new Cache();
        }

        public Dictionary<string, CachedChart> Charts = new Dictionary<string, CachedChart>();
        public string Version = CacheVersion;
        
        public Chart LoadChart(CachedChart c)
        {
            Chart m = Chart.FromFile(c.GetFileIdentifier()); //could be null
            if (m != null)
            {
                CacheChart(m); //refreshes data in case the chart has been externally modified or diff calc has changed
            }
            return m;
        }

        public void CacheChart(Chart c)
        {
            string id = c.GetFileIdentifier();
            lock (this) { Charts[id] = CachedChart.FromChart(c); }
        }

        public void Save()
        {
            lock (this) { Utils.SaveObject(this, GetCachePath()); }
        }
    }
}
