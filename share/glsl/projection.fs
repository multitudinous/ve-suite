
uniform vec3 glowColor;

uniform float alpha;
uniform float nearPlane;
uniform float farPlane;

//uniform float focalDistance;
//uniform float focalRange;

//varying float fDepth;

//varying vec3 lightPos;
//varying vec3 normal;

//varying vec4 eyePos;

void main()
{
    //vec3 N = normalize( normal );
    //vec3 L = normalize( lightPos );
    //float NDotL = max( dot( N, L ), 0.0 );

    //vec3 V = normalize( eyePos.xyz );
    //vec3 R = reflect( V, N );
    //float RDotL = max( dot( R, L ), 0.0 );

    //vec3 totalAmbient = gl_LightSource[ 0 ].ambient.rgb * gl_Color.rgb;
    //vec3 totalDiffuse = gl_LightSource[ 0 ].diffuse.rgb * gl_Color.rgb * NDotL;
    //vec3 totalSpecular =
        //gl_LightSource[ 0 ].specular.rgb * gl_Color.rgb * pow( RDotL, 15.0 );

    vec2 projectionUV = gl_TexCoord[ 0 ].st / gl_TexCoord[ 0 ].q;
    vec4 color0 = vec4( gl_Color.rgb, alpha );

    //If in frustum
    if( projectionUV.s >= 0.0 && projectionUV.s <= 1.0 &&
        projectionUV.t >= 0.0 && projectionUV.t <= 1.0 &&
        gl_TexCoord[ 0 ].q >= nearPlane &&
        gl_TexCoord[ 0 ].q <= farPlane )
    {
        color0.a = 1.0;
    }

    //float tempFocalRange = 2.0 / focalRange;
    //float tempSat =  abs( fDepth - focalDistance ) * tempFocalRange;
    //float blur = saturate( tempSat );
    //float blur = clamp( tempSat, 0.0, 1.0 );
    //vec4 color1 = vec4( fDepth, blur, 0.0, 1.0 );

    gl_FragData[ 0 ] = color0;
    gl_FragData[ 1 ] = vec4( glowColor, gl_FragData[ 0 ].a );
}
