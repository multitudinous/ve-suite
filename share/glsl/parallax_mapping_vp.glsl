uniform mat4 osg_ViewMatrixInverse;

attribute vec3 rm_Tangent;
attribute vec3 rm_Binormal;

varying vec3 vLightVector;
varying vec3 vHalfAngle;

void main()
{
    gl_Position = ftransform();

    vec4 eyePosition = gl_ModelViewMatrix * gl_Vertex;
    gl_TexCoord[ 0 ].xy = gl_Vertex.xy;

    vec3 tangent = rm_Tangent.xyz;
    vec3 normal = gl_Normal.xyz;
    vec3 binormal = rm_Binormal.xyz;

    vec4 lightPosition = vec4( 0.0, 0.0, 10000.0, 1.0 );
    vec3 temp_light_position = vec3( vec4( lightPosition.x, lightPosition.y, -lightPosition.z, lightPosition.w ) * osg_ViewMatrixInverse );
    vec3 temp_light_vector = temp_light_position.xyz - gl_Vertex.xyz;
    vLightVector.x = dot( temp_light_vector, tangent );
    vLightVector.y = dot( temp_light_vector, binormal );
    vLightVector.z = dot( temp_light_vector, normal );

    vec4 oglEyePos = eyePosition;
    oglEyePos.z = -oglEyePos.z;
    vec3 temp_eye_position = vec3( oglEyePos * osg_ViewMatrixInverse ) ;
    vec3 temp_view_vector  = temp_eye_position - gl_Vertex.xyz;
    vec3 temp_view_vector2;
    temp_view_vector2.x = dot( temp_view_vector, tangent );
    temp_view_vector2.y = dot( temp_view_vector, binormal );
    temp_view_vector2.z = dot( temp_view_vector, normal );

    vHalfAngle = vLightVector + temp_view_vector2;
}