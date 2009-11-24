//uniform mat4 osg_ViewMatrixInverse;

attribute vec4 a_tangent;
attribute vec4 a_binormal;

varying vec3 v_lightVector;
varying vec3 v_viewVector;

void main()
{
    //
    gl_Position = ftransform();

    //Get the texture coordinates
    gl_TexCoord[ 0 ] = gl_TextureMatrix[ 0 ] * gl_MultiTexCoord0;

    //Convert the vertex position into eye coordinates
    vec3 ecPosition = vec3( gl_ModelViewMatrix * gl_Vertex );

    //Convert tangent, binormal, and normal into eye coordinates
    mat3 TBNMatrix = mat3( gl_ModelViewMatrix ) *
                     mat3( a_tangent.xyz, a_binormal.xyz, gl_Normal );

    //Convert light vector into tangent space
    v_lightVector = gl_LightSource[ 0 ].position.xyz - ecPosition;
    v_lightVector *= TBNMatrix;

    //Convert view vector into tangent space
    v_viewVector = ecPosition;
    v_viewVector *= TBNMatrix;
}
