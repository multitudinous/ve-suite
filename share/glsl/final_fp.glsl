uniform float glowStrength;
uniform vec4 glowColor;

uniform sampler2D baseMap;
uniform sampler2D glowMap;

void main()
{
    vec4 original = texture2D( baseMap, gl_TexCoord[ 0 ].xy );
    float glowValue = texture2D( glowMap, gl_TexCoord[ 0 ].xy ).x;
    vec4 glow = ( glowStrength * glowValue ) * glowColor;

    gl_FragColor = original + ( 1.0 - original.w ) * glow;
}
