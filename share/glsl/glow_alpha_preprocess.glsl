uniform sampler2D glowMap;

void main()
{
    vec4 color = texture2D( glowMap, gl_TexCoord[ 0 ].st );

    gl_FragColor = color;
}
