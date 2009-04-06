uniform float time;

varying vec4 vpos, lpos;
varying vec2 wave0, wave1, wave2, vTexCoords;

void main()
{
    gl_Position = ftransform();

    //Transform vertex coordinates into -1 to 1 space
    vec3 vertex = normalize( gl_Vertex ).xyz;
    //Transform vertex coordinates into -0.5 to 0.5
    vertex.xy *= 0.5;
    //Transform vertex coordinates into 0 to 1 space
    vertex.xy += 0.5;

    vec3 startPoint = vec3( 0.5, 0.5, 0.0 );
    vec3 vecNear = vertex - startPoint;
    vec3 vecFar = -startPoint;
    vecFar.z += 1.0;
    float distance = vecFar.z / vecNear.z;
    vec3 endPoint = vec3( vecNear.x * distance, vecNear.y * distance, 1.0 );

    //Generate texture coordinates
    float scale = 0.05;
    vTexCoords = endPoint.xy * scale;

    //Scale texture coordinates to get mix of low/high frequency details
    float speed = 0.003;
    wave0 = vTexCoords * 2.0 + time * speed * vec2( 0.5, 1.0 );
    wave1 = vTexCoords * 6.0 + time * speed * vec2( 4.0, 2.0 );
    wave2 = vTexCoords * 8.0 + time * speed * vec2( 4.0, 8.0 );

    //Perspective corrected projection
    vpos = gl_Vertex.xzyw;
    lpos = gl_LightSource[ 0 ].position;

    //Get texture coordinates for _skyTexture
    gl_TexCoord[ 0 ].st = gl_MultiTexCoord0.st;

    //Get texture coordinates for _sunTexture
    gl_TexCoord[ 1 ].s = dot( gl_Vertex, gl_ObjectPlaneS[ 1 ] );
    gl_TexCoord[ 1 ].t = dot( gl_Vertex, gl_ObjectPlaneT[ 1 ] );
    gl_TexCoord[ 1 ].p = dot( gl_Vertex, gl_ObjectPlaneR[ 1 ] );
    gl_TexCoord[ 1 ].q = dot( gl_Vertex, gl_ObjectPlaneQ[ 1 ] );
}
