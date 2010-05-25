uniform sampler2D tex;
//uniform float opacityVal;
uniform vec3 glowColor;
uniform uint texUnit;
void main()
{
    vec4 colorTex = texture2D(tex, gl_TexCoord[texUnit].st);
    //colorTex.a = opacityVal;
    gl_FragData[ 0 ] = colorTex * gl_Color;

    //To handle the glow
    gl_FragData[ 1 ] = vec4( 0, 0, 0, 1 );
}