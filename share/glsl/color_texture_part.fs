uniform sampler2D tex;
uniform vec3 glowColor;
uniform vec4 partColor;
//uniform uint texUnit;

void main()
{
    vec4 colorTex = texture2D(tex, gl_TexCoord[0].st);
    gl_FragData[ 0 ] = colorTex * partColor;

    //To handle the glow
    gl_FragData[ 1 ] = vec4( 0, 0, 0, 1 );
}