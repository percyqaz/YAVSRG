﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Interlude.Interface.Animations
{
    public class AnimationGroup : Animation
    {
        private List<Animation> items = new List<Animation>();
        private bool temporary;

        public AnimationGroup(bool permanent)
        {
            temporary = !permanent;
        }

        public void Add(Animation a)
        {
            items.Add(a);
        }

        public override bool DisposeMe
        {
            get
            {
                return temporary && !Running;
            }
        }

        public override bool Running
        {
            get
            {
                foreach (Animation a in items)
                {
                    if (a.Running) return true;
                };
                return false;
            }
        }

        public override void Update()
        {
            int c = items.Count;
            int i = 0;
            while (i < c)
            {
                if (items[i].DisposeMe)
                {
                    items.RemoveAt(i);
                    c--;
                    continue;
                }
                items[i].Update();
                i++;
            }
        }
    }
}
