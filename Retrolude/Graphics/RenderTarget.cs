﻿using OpenTK;
using System.Drawing;
using Interlude.Interface;

namespace Interlude.Graphics
{
    public struct RenderTarget
    {
        public Vector2 Coord1, Coord2, Coord3, Coord4;

        public Vector2 Texcoord1, Texcoord2, Texcoord3, Texcoord4;

        public Color Color1, Color2, Color3, Color4;

        public Sprite Texture;

        public RenderTarget(Sprite texture, Rect bounds, Color color, int ux = 0, int uy = 0)
        {
            Coord1 = new Vector2(bounds.Left, bounds.Top);
            Coord2 = new Vector2(bounds.Right, bounds.Top);
            Coord3 = new Vector2(bounds.Right, bounds.Bottom);
            Coord4 = new Vector2(bounds.Left, bounds.Bottom);

            float x = (float)texture.Width / texture.SourceWidth / texture.Columns;
            float y = (float)texture.Height / texture.SourceHeight / texture.Rows;
            float ox = (float)texture.Offset_X / texture.SourceWidth;
            float oy = (float)texture.Offset_Y / texture.SourceHeight;

            ux = ux % texture.Columns;
            uy = uy % texture.Rows;

            Texcoord1 = new Vector2(ox + x * ux, oy + y * uy);
            Texcoord2 = new Vector2(ox + x + x * ux,  oy + y * uy);
            Texcoord3 = new Vector2(ox + x + x * ux, oy + y + y * uy);
            Texcoord4 = new Vector2(ox + x * ux, oy + y + y * uy);

            Color1 = Color2 = Color3 = Color4 = color;

            Texture = texture;
        }

        public RenderTarget(Sprite texture, Rect bounds, Color color, Vector2 uv1, Vector2 uv2, Vector2 uv3, Vector2 uv4)
        {
            Coord1 = new Vector2(bounds.Left, bounds.Top);
            Coord2 = new Vector2(bounds.Right, bounds.Top);
            Coord3 = new Vector2(bounds.Right, bounds.Bottom);
            Coord4 = new Vector2(bounds.Left, bounds.Bottom);

            Texcoord1 = uv1;
            Texcoord2 = uv2;
            Texcoord3 = uv3;
            Texcoord4 = uv4;

            Color1 = Color2 = Color3 = Color4 = color;

            Texture = texture;
        }

        public RenderTarget(Rect bounds, Color col1, Color col2, Color col3, Color col4)
        {
            Coord1 = new Vector2(bounds.Left, bounds.Top);
            Coord2 = new Vector2(bounds.Right, bounds.Top);
            Coord3 = new Vector2(bounds.Right, bounds.Bottom);
            Coord4 = new Vector2(bounds.Left, bounds.Bottom);

            Color1 = col1;
            Color2 = col2;
            Color3 = col3;
            Color4 = col4;

            Texcoord1 = Texcoord2 = Texcoord3 = Texcoord4 = default;

            Texture = Sprite.Default;
        }

        public RenderTarget(Sprite texture, Rect bounds, Color col1, Color col2, Color col3, Color col4, Vector2 uv1, Vector2 uv2, Vector2 uv3, Vector2 uv4)
        {
            Coord1 = new Vector2(bounds.Left, bounds.Top);
            Coord2 = new Vector2(bounds.Right, bounds.Top);
            Coord3 = new Vector2(bounds.Right, bounds.Bottom);
            Coord4 = new Vector2(bounds.Left, bounds.Bottom);

            Texcoord1 = uv1;
            Texcoord2 = uv2;
            Texcoord3 = uv3;
            Texcoord4 = uv4;

            Color1 = col1;
            Color2 = col2;
            Color3 = col3;
            Color4 = col4;

            Texture = texture;
        }

        public RenderTarget(Sprite texture, Vector2 pos1, Vector2 pos2, Vector2 pos3, Vector2 pos4, Color col1, Color col2, Color col3, Color col4, Vector2 uv1, Vector2 uv2, Vector2 uv3, Vector2 uv4)
        {
            Coord1 = pos1;
            Coord2 = pos2;
            Coord3 = pos3;
            Coord4 = pos4;

            Texcoord1 = uv1;
            Texcoord2 = uv2;
            Texcoord3 = uv3;
            Texcoord4 = uv4;

            Color1 = col1;
            Color2 = col2;
            Color3 = col3;
            Color4 = col4;

            Texture = texture;
        }

        public RenderTarget(Vector2 pos1, Vector2 pos2, Vector2 pos3, Vector2 pos4, Color col)
        {
            Coord1 = pos1;
            Coord2 = pos2;
            Coord3 = pos3;
            Coord4 = pos4;

            Texcoord1 = Texcoord2= Texcoord3 = Texcoord4 = default;

            Color1 = col;
            Color2 = col;
            Color3 = col;
            Color4 = col;

            Texture = Sprite.Default;
        }

        public Vector2 GetTexCoord(int i)
        {
            switch (i)
            {
                case 3: return Texcoord4;
                case 2: return Texcoord3;
                case 1: return Texcoord2;
                case 0: default: return Texcoord1;
            }
        }

        public RenderTarget Rotate(int r)
        {
            return new RenderTarget(Texture, Coord1, Coord2, Coord3, Coord4, Color1, Color2, Color3, Color4, GetTexCoord(r % 4), GetTexCoord((r + 1) % 4), GetTexCoord((r + 2) % 4), GetTexCoord((r + 3) % 4));
        }
    }
}
