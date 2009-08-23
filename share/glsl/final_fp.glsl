uniform float glowStrength;

uniform sampler2D baseMap;
uniform sampler2D stencilMap;
uniform sampler2D glowMap;

void main()
{
    vec3 base = texture2D( baseMap, gl_TexCoord[ 0 ].xy ).rgb;
    vec3 glow = texture2D( glowMap, gl_TexCoord[ 0 ].xy ).rgb;
    glow *= glowStrength;

    vec3 stencil = texture2D( stencilMap, gl_TexCoord[ 0 ].xy ).rgb;
    float stencilGlowValue = clamp( length( stencil ), 0.0, 1.0 ) * 0.9;
    glow = ( 1.0 - stencilGlowValue ) * glow;

    gl_FragColor = vec4( base + glow, 1.0 );
}
