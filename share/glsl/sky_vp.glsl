uniform float time;

varying vec4 vpos, lpos;
varying vec2 wave0, wave1, wave2;

void main()
{
    //Generate texture coordinates
    float size = 0.0000000005;
    vec2 vTexCoords = gl_Vertex.xy * size;

    //Scale texture coordinates to get mix of low/high frequency details
    float speed = 1.0;
    wave0 = vTexCoords * 2.0 + time * speed * vec2( 0.5, 1.0 );
    wave1 = vTexCoords * 6.0 + time * speed * vec2( 2.0, 1.0 );
    wave2 = vTexCoords * 8.0 + time * speed * vec2( 2.0, 4.0 );

    //Perspective corrected projection
    gl_Position = ftransform();
    vpos = gl_Vertex.xzyw;
    lpos = gl_LightSource[ 0 ].position;
}
