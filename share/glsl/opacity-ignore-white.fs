uniform sampler2D tex;
uniform float opacityVal;
uniform vec4 glowColor;

//This shader is specifically setup to ignore the white part of the texture
//in the vesuite textual display dialog.
void main()
{
    vec4 texColor = texture2D(tex, gl_TexCoord[0].st);
    //Ignore the white color in the texture
    if( texColor.r == 1.0 &&  texColor.g == 1.0 &&  texColor.b == 1.0 )
    {
        texColor.a = 1.0;
    }
    else
    {
        texColor.a = opacityVal;
    }
	gl_FragData[ 0 ] = texColor;

    //To handle the glow 
    vec4 color = glowColor;
    if( gl_Color.a < 1.0 )
    { 
        color.a = gl_Color.a;
    }

    gl_FragData[ 1 ] = color;
}
