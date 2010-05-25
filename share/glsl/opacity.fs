uniform sampler2D tex;
uniform float opacityVal;
uniform vec3 glowColor;

void main()
{
    vec4 texColor = texture2D(tex, gl_TexCoord[0].st);
    texColor.a = opacityVal;
    gl_FragData[ 0 ] = texColor;

    //To handle the glow
    gl_FragData[ 1 ] = vec4( glowColor, gl_Color.a );
}
