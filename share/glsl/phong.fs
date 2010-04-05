varying vec4 eyePos;
varying vec3 lightPos;
varying vec3 normal;

void main()
{
    vec3 N = normalize( normal );
    vec3 L = normalize( lightPos );
    float NDotL = max( dot( N, L ), 0.0 );

    vec3 V = normalize( eyePos.xyz );
    vec3 R = reflect( V, N );
    float RDotL = max( dot( R, L ), 0.0 );

    vec3 totalAmbient = gl_LightSource[ 0 ].ambient.rgb * gl_Color.rgb;
    vec3 totalDiffuse = gl_LightSource[ 0 ].diffuse.rgb * gl_Color.rgb * NDotL;
    vec3 totalSpecular =
        gl_LightSource[ 0 ].specular.rgb * gl_Color.rgb * pow( RDotL, 15.0 );

    gl_FragColor = vec4( totalAmbient + totalDiffuse + totalSpecular, 1.0 );
}