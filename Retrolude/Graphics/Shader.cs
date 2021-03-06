﻿using OpenTK.Graphics.OpenGL;
using Prelude.Utilities;

namespace Interlude.Graphics
{
    public class Shader
    {
        public int VertexShader;
        public int FragmentShader;
        public int Program;

        public Shader(string vs, string fs)
        {
            FragmentShader = GL.CreateShader(ShaderType.FragmentShader);
            VertexShader = GL.CreateShader(ShaderType.VertexShader);
            GL.ShaderSource(FragmentShader, fs);
            GL.ShaderSource(VertexShader, vs);
            GL.CompileShader(FragmentShader);
            if (GL.GetError() != ErrorCode.NoError)
            {
                Logging.Log("Couldn't compile fragment shader", GL.GetError().ToString(), Logging.LogType.Error);
            }
            GL.CompileShader(VertexShader);
            if (GL.GetError() != ErrorCode.NoError)
            {
                Logging.Log("Couldn't compile vertex shader", GL.GetError().ToString(), Logging.LogType.Error);
            }

            Program = GL.CreateProgram();
            GL.AttachShader(Program, FragmentShader);
            //GL.AttachShader(Program, VertexShader);

            GL.LinkProgram(Program);
        }
    }
}
