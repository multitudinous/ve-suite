
varying float fDepth;

varying vec3 lightPos;
varying vec3 normal;

varying vec4 eyePos;

void main()
{
    gl_Position = ftransform();

    lightPos = gl_LightSource[ 0 ].position.xyz;
    normal = vec3( gl_NormalMatrix * gl_Normal );
    eyePos = gl_ModelViewMatrix * gl_Vertex;

    fDepth = -eyePos.z;

    gl_FrontColor = gl_Color;

    gl_TexCoord[ 0 ].s = dot( eyePos, gl_EyePlaneS[ 0 ] );
    gl_TexCoord[ 0 ].t = dot( eyePos, gl_EyePlaneT[ 0 ] );
    gl_TexCoord[ 0 ].q = dot( eyePos, gl_EyePlaneQ[ 0 ] );
}
