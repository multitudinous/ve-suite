uniform sampler2D ColorMap;
uniform sampler2D BumpMap;
uniform sampler2D skyMap;

varying vec4 vpos, lpos;
varying vec2 wave0, wave1, wave2;

const vec4 dark_blue = vec4( 0.00, 0.50, 0.80, 0.0 );
const vec4 light_cyan = vec4( 0.89, 1.00, 1.00, 0.0 );
const vec4 white = vec4( 1.00, 1.00, 1.00, 0.0 );
const vec4 grey = vec4( 0.50, 0.60, 0.60, 0.0 );

void main()
{
    //x = Cloud cover percentage, y = Cloud sharpness
    float cCoverPercentage = 0.38;
    float cSharpness = 0.0006;

    //Compute normal (assumes this is a sphere)
    vec3 norm = -normalize( vpos ).xyz;

    //Normalise light vector
    vec3 light = normalize( vec3( lpos.x, lpos.y, lpos.z ) );

    //Get bump layers
    vec3 vBumpTexA = texture2D( BumpMap, wave0 ).xyz;
    vec3 vBumpTexB = 0.5 * texture2D( BumpMap, wave1 ).xyz;
    vec3 vBumpTexC = 0.25 * texture2D( BumpMap, wave2 ).xyz;

    //Average bump layers to get cloud normal
    vec3 cNorm =
        normalize( vBumpTexA.xyz + vBumpTexB.xyz + vBumpTexC.xyz );

    //Compute clouds diffuse lighting
    float cDiff = max( dot( cNorm, light ), 0.0 );

    //Get diffuse layers
    float vDiffTexA = texture2D( ColorMap, wave0 ).x;
    float vDiffTexB = 0.5 * texture2D( ColorMap, wave1 ).x;
    float vDiffTexC = 0.25 * texture2D( ColorMap, wave2 ).x;

    //Average diffuse layers
    float sky = ( vDiffTexA + vDiffTexB + vDiffTexC ) / 3.0;

    //Compute cloud density
    float cDens =
        1.0 - pow( cSharpness, max( ( sky - cCoverPercentage ), 0.0 ) );

    //Gradually change color to simulate scattering
    vec4 skyColor =
        mix( dark_blue, light_cyan,
            0.1 * pow( 11.0, length( norm.xz ) ) - 0.1 );
    //vec4 skyColor = texture2D( skyMap, );

    //Mix between sky and cloud color depending on density
    skyColor = mix( skyColor, white * cDiff, cDens );

    //Add a horizon haze
    float haze = pow( gl_FragCoord.z * ( 1.0 + norm.y ), 10.0 );
    haze = pow( 2.71828, -10.0 * haze );
    skyColor = mix( grey, skyColor, haze );

    //Compute sun light
    vec4 sunlight =
        vec4( pow( max( 0.0, dot( -light, norm ) ), 1024.0 ) );

    gl_FragColor = skyColor + cCoverPercentage * sunlight;
}
