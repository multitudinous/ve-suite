uniform float WT9_0;
uniform float WT9_1;
uniform float WT9_2;
uniform float WT9_3;
uniform float WT9_4;

uniform sampler2D glowMap;

void main()
{
    float WT9_NORMALIZE = ( WT9_0 + 2.0 * ( WT9_1 + WT9_2 + WT9_3 + WT9_4 ) );
    float outColor = texture2D( glowMap, gl_TexCoord[ 0 ].xy ).w * ( WT9_1 / WT9_NORMALIZE );
    outColor      += texture2D( glowMap, gl_TexCoord[ 1 ].xy ).w * ( WT9_2 / WT9_NORMALIZE );
    outColor      += texture2D( glowMap, gl_TexCoord[ 2 ].xy ).w * ( WT9_3 / WT9_NORMALIZE );
    outColor      += texture2D( glowMap, gl_TexCoord[ 3 ].xy ).w * ( WT9_4 / WT9_NORMALIZE );
    outColor      += texture2D( glowMap, gl_TexCoord[ 4 ].xy ).w * ( WT9_0 / WT9_NORMALIZE );
    outColor      += texture2D( glowMap, gl_TexCoord[ 0 ].zw ).w * ( WT9_1 / WT9_NORMALIZE );
    outColor      += texture2D( glowMap, gl_TexCoord[ 1 ].zw ).w * ( WT9_2 / WT9_NORMALIZE );
    outColor      += texture2D( glowMap, gl_TexCoord[ 2 ].zw ).w * ( WT9_3 / WT9_NORMALIZE );
    outColor      += texture2D( glowMap, gl_TexCoord[ 3 ].zw ).w * ( WT9_4 / WT9_NORMALIZE );

    gl_FragColor = vec4( outColor, outColor, outColor, outColor );   
}

