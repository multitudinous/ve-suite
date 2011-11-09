uniform vec3 fvLightPosition;
uniform vec3 fvEyePosition;

varying vec2 TexcoordLayerZero;
varying vec2 TexcoordLayerOne;
varying vec3 ViewDirection;
varying vec3 LightDirection;
   
attribute vec3 rm_Binormal;
attribute vec3 rm_Tangent;
   
void main( void )
{
   gl_Position = ftransform();
   TexcoordLayerZero    = gl_MultiTexCoord0.xy;
   TexcoordLayerOne     = gl_MultiTexCoord1.xy;
    
   vec4 fvObjectPosition = gl_ModelViewMatrix * gl_Vertex;
   
   vec3 fvViewDirection  = fvEyePosition - fvObjectPosition.xyz;
   vec3 fvLightDirection = fvLightPosition - fvObjectPosition.xyz;
     
   vec3 fvNormal         = gl_NormalMatrix * gl_Normal;
   vec3 fvBinormal       = gl_NormalMatrix * rm_Binormal;
   vec3 fvTangent        = gl_NormalMatrix * rm_Tangent;
      
   // for specular reflections
   ViewDirection.x  = dot( fvTangent, fvViewDirection );
   ViewDirection.y  = dot( fvBinormal, fvViewDirection );
   ViewDirection.z  = dot( fvNormal, fvViewDirection );
   
   LightDirection.x  = dot( fvTangent, fvLightDirection.xyz );
   LightDirection.y  = dot( fvBinormal, fvLightDirection.xyz );
   LightDirection.z  = dot( fvNormal, fvLightDirection.xyz );
   
}