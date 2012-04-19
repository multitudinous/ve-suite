#version 120

uniform vec3 lightDir = vec3(.577, .577, .577);
uniform vec3 color = vec3(1,.4,.3);
void main()
{
    

    // calculate normal from texture coordinates
    vec3 N;
    N.xy = gl_TexCoord[0].xy*vec2(2.0, -2.0) + vec2(-1.0, 1.0);
    float mag = dot(N.xy, N.xy);
    if (mag > 1) discard;   // kill pixels outside circle
    N.z = sqrt(1-mag);
    // calculate lighting
    //float diffuse = max(0.0, dot(lightDir, N));
    //vec3 color2 = vec3(1,0,0);
    float diffuse = .2+ max(0.0, dot(vec3(.577,.577,.577), N));
    float ndcDepth = (2.0 * gl_FragCoord.z - gl_DepthRange.near - gl_DepthRange.far) / (gl_DepthRange.far - gl_DepthRange.near);
    float clipDepth = ndcDepth/gl_FragCoord.w;
    vec4 cameraSpacePosition = gl_ProjectionMatrixInverse * vec4(0, 0, clipDepth, gl_FragCoord.z);
    cameraSpacePosition.z += N.z;
    vec4 clipPos = gl_ProjectionMatrix * cameraSpacePosition;
    ndcDepth = clipPos.z / clipPos.w;
    gl_FragDepth = ((gl_DepthRange.diff * ndcDepth) +
    gl_DepthRange.near + gl_DepthRange.far) / 2.0;

    gl_FragDepth = gl_FragCoord.z + .000001;
    gl_FragData[ 0 ] = gl_Color * (diffuse);
    //gl_FragData[ 0] = gl_Color;
    //gl_FragColor = vec4(1,0,0,1)*diffuse;
    //This has to be here because OSG is awesome! (don't remove it)
    gl_FragData[ 1 ] = vec4( 0.0, 0.0, 0.0, 1.0 );
}
