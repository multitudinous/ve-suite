uniform float glowStrength;

uniform sampler2D baseMap;
uniform sampler2D stencilMap;
uniform sampler2D glowMap;

void main()
{
    vec4 base = texture2D( baseMap, gl_TexCoord[ 0 ].xy );
    vec4 glow = texture2D( glowMap, gl_TexCoord[ 0 ].xy );
    glow *= glowStrength;

    vec4 stencil = texture2D( stencilMap, gl_TexCoord[ 0 ].xy );
    float stencilGlowValue = clamp( length( stencil.rgb ), 0.0, 1.0 ) * 0.9;
    glow = ( 1.0 - stencilGlowValue ) * glow;

    gl_FragColor = base + glow;
}
