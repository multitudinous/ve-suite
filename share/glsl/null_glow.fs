//uniform vec3 glowColor;

void main()
{
    gl_FragData[ 0 ] = gl_Color;

    //To handle the glow
    gl_FragData[ 1 ] = vec4( 0, 0, 0, 1 );
}
