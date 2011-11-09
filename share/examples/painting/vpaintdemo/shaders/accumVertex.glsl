// glslv output by Cg compiler
// cgc version 3.0.0016, build date Feb 11 2011
// command line args: -profile glslv
// source file: accumVertex.cg
//vendor NVIDIA Corporation
//version 3.0.0.16
//profile glslv
//program main
//semantic main.modelToWorld
//semantic main.ModelViewProjMatrix
//semantic main.lightPosition
//semantic main.eyePosition
//var float4x4 modelToWorld :  : _modelToWorld1[0], 4 : 1 : 1
//var float4x4 ModelViewProjMatrix :  : _ModelViewProjMatrix1[0], 4 : 2 : 1
//var float3 lightPosition :  : _lightPosition1 : 3 : 1
//var float3 eyePosition :  : _eyePosition1 : 4 : 1
//var float3 IN.texcoord : $vin.TEXCOORD0 : ATTR8 : 0 : 1
//var float4 IN.position : $vin.POSITION : ATTR0 : 0 : 1
//var float3 main.texcoord : $vout.TEXCOORD0 : TEX0 : -1 : 1
//var float4 main.position : $vout.POSITION : HPOS : -1 : 1
//var float3 main.positionW : $vout.TEXCOORD4 : TEX4 : -1 : 1
//var float3 main.N : $vout.TEXCOORD5 : TEX5 : -1 : 1
//var float3 main.L : $vout.TEXCOORD3 : TEX3 : -1 : 1
//var float3 main.V : $vout.TEXCOORD2 : TEX2 : -1 : 1

varying vec3 _texcoord;
varying vec3 _N;
varying vec3 _L;
varying vec3 _V;
// varying vec3 _positionW; // not used

uniform vec4 _modelToWorld1[4];
//uniform vec4 _ModelViewProjMatrix1[4]; // not used
uniform vec3 _lightPosition1;
uniform vec3 _eyePosition1;
vec4 _r0007;
vec4 _r0017; // not used

 // main procedure, the original name was main
void main()
{
	// fetch texcoords for texture 0 so they can be interpolated by varyings
    _texcoord = vec3(float(gl_MultiTexCoord0.x), float(gl_MultiTexCoord0.y), float(gl_MultiTexCoord0.z));

    // calculate some intermediate values for vertex transform and eye/light vectors

	// transform vertex
/*
	_r0007.x = dot(_ModelViewProjMatrix1[0], gl_Vertex);
    _r0007.y = dot(_ModelViewProjMatrix1[1], gl_Vertex);
    _r0007.z = dot(_ModelViewProjMatrix1[2], gl_Vertex);
    _r0007.w = dot(_ModelViewProjMatrix1[3], gl_Vertex);
*/
	_r0007 = ftransform(); // aka gl_ProjectionMatrix * gl_ModelViewMatrix * gl_Vertex;

    _r0017.x = dot(_modelToWorld1[0], gl_Vertex);
    _r0017.y = dot(_modelToWorld1[1], gl_Vertex);
    _r0017.z = dot(_modelToWorld1[2], gl_Vertex);

	// Vector from vertex to light
    _L = _lightPosition1 - _r0017.xyz;
	// Vector from vertex to view
    _V = _eyePosition1 - _r0017.xyz;

    //_positionW = _r0017.xyz; // not used

	// "Normal (in world space)"
    _N = vec3( 0.00000000E+000, 0.00000000E+000, 1.00000000E+000);

    gl_Position = _r0007;
    return;
} // main end
