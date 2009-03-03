uniform float time;

varying vec4 vpos, lpos;
varying vec2 wave0, wave1, wave2;

void main()
{
    gl_Position = ftransform();

    //Generate texture coordinates
    float scale = 0.000000001;
    vec2 vTexCoords = gl_Vertex.xy * scale;

    //Scale texture coordinates to get mix of low/high frequency details
    float speed = 0.005;
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
