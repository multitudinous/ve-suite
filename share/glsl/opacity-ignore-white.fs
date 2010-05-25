uniform sampler2D tex;
uniform float opacityVal;
uniform vec3 glowColor;
uniform bool ignoreWhite;

//This shader is specifically setup to ignore the white part of the texture
//in the vesuite textual display dialog.
void main()
{
    vec4 texColor = texture2D(tex, gl_TexCoord[0].st);
    texColor.a = opacityVal;
    //Ignore the white color in the texture
    if( ignoreWhite )
    {
        if( texColor.r == 1.0 && texColor.g == 1.0 && texColor.b == 1.0 )
        {
            texColor.a = 1.0;
        }
    }
    gl_FragData[ 0 ] = texColor;

    //To handle the glow
    gl_FragData[ 1 ] = vec4( glowColor, gl_Color.a );
}
