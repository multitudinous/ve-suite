uniform vec2 quadScreenSize;

void main()
{
    gl_Position = ftransform();

    float texelIncrement = 1.0 / quadScreenSize.x;
    gl_TexCoord[ 0 ] = vec4( gl_MultiTexCoord0.x + texelIncrement * 1.0, gl_MultiTexCoord0.y,
                             gl_MultiTexCoord0.x - texelIncrement * 1.0, gl_MultiTexCoord0.y );
    gl_TexCoord[ 1 ] = vec4( gl_MultiTexCoord0.x + texelIncrement * 2.0, gl_MultiTexCoord0.y,
                             gl_MultiTexCoord0.x - texelIncrement * 2.0, gl_MultiTexCoord0.y );
    gl_TexCoord[ 2 ] = vec4( gl_MultiTexCoord0.x + texelIncrement * 3.0, gl_MultiTexCoord0.y,
                             gl_MultiTexCoord0.x - texelIncrement * 3.0, gl_MultiTexCoord0.y );
    gl_TexCoord[ 3 ] = vec4( gl_MultiTexCoord0.x + texelIncrement * 4.0, gl_MultiTexCoord0.y,
                             gl_MultiTexCoord0.x - texelIncrement * 4.0, gl_MultiTexCoord0.y );
    gl_TexCoord[ 4 ] = vec4( gl_MultiTexCoord0.x,                        gl_MultiTexCoord0.y, 1.0, 1.0 );
}

