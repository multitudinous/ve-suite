
varying vec4 eyePos;
varying vec3 lightPos;
varying vec3 normal;

void main()
{
    gl_Position = ftransform();

    eyePos = gl_ModelViewMatrix * gl_Vertex;
    lightPos = gl_LightSource[ 0 ].position.xyz;
    normal = vec3( gl_NormalMatrix * gl_Normal );

    gl_FrontColor = gl_Color;
}
