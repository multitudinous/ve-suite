
uniform mat4 viewProj;
uniform mat4 celestialOrientation;

varying vec2 oTC;

void main()
{
    // TBD GL3
    oTC = gl_MultiTexCoord0.st;
    // TBD GL3
    gl_Position = viewProj * celestialOrientation * gl_Vertex;
}
