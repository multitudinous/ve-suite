uniform sampler2D tex;
uniform float opacityVal;
uniform vec4 glowColor;

void main()
{
    vec4 texColor = texture2D(tex, gl_TexCoord[0].st);
    texColor.a = opacityVal;
	gl_FragData[ 0 ] = texColor;

    //To handle the glow 
    vec4 color = glowColor;
    if( gl_Color.a < 1.0 )
    { 
        color.a = gl_Color.a;
    }

    gl_FragData[ 1 ] = color;
}
