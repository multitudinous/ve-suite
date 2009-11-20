uniform sampler2D baseMap;
uniform sampler2D bumpMap;

varying vec3 vLightVector;
varying vec3 vHalfAngle;

void main()
{
    vec3 base = texture2D( baseMap, gl_TexCoord[ 0 ].xy ).xyz;
    vec3 bump = texture2D( bumpMap, gl_TexCoord[ 0 ].xy ).xyz;

    vec3 normalized_light_vector = normalize( vLightVector );
    vec3 normalized_half_angle   = normalize( vHalfAngle );

    vec3 smoothOut = vec3( 0.5, 0.5, 1.0 );

    float bumpiness = 1.0;
    bump = mix( smoothOut, bump, bumpiness );
    bump = normalize( ( bump * 2.0 ) - 1.0 );

    vec3 n_dot_l = vec3( dot( bump, normalized_light_vector ) );
    vec3 n_dot_h = vec3( dot( bump, normalized_half_angle ) );

    float Ka = 0.3;
    float Kd = 1.0;
    float Ks = 1.0;
    float specular_power = 64.0;
    vec4 ambient = vec4( 0.9255, 0.9380, 0.9380, 1.0 );
    vec4 diffuse = vec4( 0.8392, 0.8623, 0.8904, 1.0 );
    vec4 specular = vec4( 1.0, 0.9567, 0.6704, 1.0 );
    vec3 color0 =
        ( base * ambient.xyz * Ka ) +
        ( base * diffuse.xyz * Kd * max( vec3( 0.0 ), n_dot_l ) ) +
        ( specular.xyz * Ks * pow( max( vec3( 0.0 ), n_dot_h ), vec3( specular_power ) ) );

    float color0_a = 1.0;

    gl_FragColor = vec4( color0, color0_a );
}