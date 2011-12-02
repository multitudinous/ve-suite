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
    gl_FragData[ 0 ] = gl_Color * (diffuse);
    //gl_FragData[ 0] = gl_Color;
    //gl_FragColor = vec4(1,0,0,1)*diffuse;
   //Why in the fuck does this have to be here? (don't remove it)
   gl_FragData[ 1 ] = vec4( 0.0, 0.0, 0.0, 1.0 );
}
