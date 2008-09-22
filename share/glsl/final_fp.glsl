uniform float glowStrength;
uniform vec4 glowColor;

uniform sampler2D baseMap;
uniform sampler2D stencilGlowMap;
uniform sampler2D glowMap;

void main()
{
    vec4 original = texture2D( baseMap, gl_TexCoord[ 0 ].xy );
    float inverseGlowAlpha = 0.9;
    float stencilGlowValue = texture2D( stencilGlowMap, gl_TexCoord[ 0 ].xy ).x * inverseGlowAlpha;
    float glowValue = texture2D( glowMap, gl_TexCoord[ 0 ].xy ).x;
    vec4 glow = ( glowStrength * glowValue ) * glowColor;
    glow = ( 1.0 - stencilGlowValue ) * glow;

    //original = dest
    //glow = src
    //src * sFactor + dest * dFactor
    //vec4 srcFactor = vec4( glow.a, glow.a, glow.a, glow.a );
    //vec4 destFactor = vec4( 1.0, 1.0, 1.0, 1.0 ) - srcFactor;
    //vec4 srcComponent = glow * srcFactor;
    //vec4 destComponent = original * destFactor;
    //gl_FragColor = srcComponent + destComponent;
    gl_FragColor = original + glow;
}
