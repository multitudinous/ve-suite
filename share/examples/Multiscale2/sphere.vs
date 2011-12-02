#version 120

uniform float pointRadius = 10.0;  // point size in world space
uniform float pointScale = 1.0;   // scale to calculate size in pixels
uniform float densityScale = 1.0;
uniform float densityOffset = 1.0;
void main()
{
    // calculate window-space point size
    vec3 posEye = vec3(gl_ModelViewMatrix * vec4(gl_Vertex.xyz, 1.0));
    float dist = length(posEye);
   // dist = 10;
   //gl_PointSize = 1;
    gl_PointSize = 2300 * (1.0 / dist);
    //gl_PointSize = 10;
    gl_TexCoord[0] = gl_MultiTexCoord0;
    gl_Position = gl_ModelViewProjectionMatrix * vec4(gl_Vertex.xyz, 1.0);

   // gl_FrontColor = vec4(1.0,1.0,1.0,1.0);
   gl_FrontColor = gl_Color;
}
