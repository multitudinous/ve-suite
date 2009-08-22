uniform vec2 quadScreenSize;

void main()
{
    gl_Position = ftransform();

    float texelIncrement = 1.0 / quadScreenSize.s;
    gl_TexCoord[ 0 ] = vec4( gl_MultiTexCoord0.st, 1.0, 1.0 );
    gl_TexCoord[ 1 ] =
        vec4( gl_MultiTexCoord0.s + texelIncrement * 1.0, gl_MultiTexCoord0.t,
              gl_MultiTexCoord0.s - texelIncrement * 1.0, gl_MultiTexCoord0.t );
    gl_TexCoord[ 2 ] =
        vec4( gl_MultiTexCoord0.s + texelIncrement * 2.0, gl_MultiTexCoord0.t,
              gl_MultiTexCoord0.s - texelIncrement * 2.0, gl_MultiTexCoord0.t );
    gl_TexCoord[ 3 ] =
        vec4( gl_MultiTexCoord0.s + texelIncrement * 3.0, gl_MultiTexCoord0.t,
              gl_MultiTexCoord0.s - texelIncrement * 3.0, gl_MultiTexCoord0.t );
    gl_TexCoord[ 4 ] =
        vec4( gl_MultiTexCoord0.s + texelIncrement * 4.0, gl_MultiTexCoord0.t,
              gl_MultiTexCoord0.s - texelIncrement * 4.0, gl_MultiTexCoord0.t );
}
