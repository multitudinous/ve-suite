//uniform vec4 glowColor;

void main()
{
    gl_FragData[ 0 ] = gl_Color;
    
    //To handle the glow 
    //vec4 color = glowColor;
    //if( gl_Color.a < 1.0 )
    //{
    //    color.a = gl_Color.a;
    //}

    gl_FragData[ 1 ] = vec4( 0, 0, 0, 1);
}
