uniform sampler2D skyMap;
uniform sampler2D sunMap;
uniform sampler2D colorMap;
uniform sampler2D bumpMap;

varying vec4 vpos, lpos;
varying vec2 wave0, wave1, wave2, vTexCoords;

const vec4 white = vec4( 1.00, 1.00, 1.00, 0.0 );
const vec4 grey = vec4( 0.80, 0.80, 0.80, 0.0 );

void main()
{
    //x = Cloud cover percentage, y = Cloud sharpness
    float cCoverPercentage = 0.34;
    float cSharpness = 0.0006;

    //Compute normal (assumes this is a sphere)
    vec3 norm = -normalize( vpos ).xyz;

    //Normalise light vector
    vec3 light = normalize( vec3( lpos.x, lpos.y, lpos.z ) );

    //Get bump layers
    vec3 vBumpTexA = texture2D( bumpMap, wave0 ).xyz;
    vec3 vBumpTexB = 0.5 * texture2D( bumpMap, wave1 ).xyz;
    vec3 vBumpTexC = 0.25 * texture2D( bumpMap, wave2 ).xyz;

    //Average bump layers to get cloud normal
    vec3 cNorm =
        normalize( vBumpTexA.xyz + vBumpTexB.xyz + vBumpTexC.xyz );

    //Compute clouds diffuse lighting
    float cDiff = max( dot( cNorm, light ), 0.0 );

    //Get diffuse layers
    float vDiffTexA = texture2D( colorMap, wave0 ).x;
    float vDiffTexB = 0.5 * texture2D( colorMap, wave1 ).x;
    float vDiffTexC = 0.25 * texture2D( colorMap, wave2 ).x;

    //Average diffuse layers
    float sky = ( vDiffTexA + vDiffTexB + vDiffTexC ) / 3.0;

    //Compute cloud density
    float cDens =
        1.0 - pow( cSharpness, max( ( sky - cCoverPercentage ), 0.0 ) );

    //Get _skyTexture
    vec4 skyColor = texture2D( skyMap, gl_TexCoord[ 0 ].st );

    //Mix between sky and cloud color depending on density
    skyColor = mix( skyColor, white * cDiff, cDens );

    //Add a horizon haze
    float haze = pow( gl_FragCoord.z * ( 1.0 + norm.y ), 4.0 );
    haze = pow( 2.71828, -4.0 * haze );
    skyColor = mix( grey, skyColor, haze );

    //Get _sunTexture using projection texture coordinates
    vec3 sunTexCoords = gl_TexCoord[ 1 ].stp / gl_TexCoord[ 1 ].q;
    vec4 sunColor = texture2DProj( sunMap, sunTexCoords );

    //Compute sun light
    vec4 sunlight =
        vec4( pow( max( 0.0, dot( -sunColor.xyz, norm ) ), 2048.0 ) );

    gl_FragColor = skyColor + cCoverPercentage * sunlight;

    //gl_FragColor = vec4( vTexCoords.x, vTexCoords.y, 0.0, 1.0 );
}
