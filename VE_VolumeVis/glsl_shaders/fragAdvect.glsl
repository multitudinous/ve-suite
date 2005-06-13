uniform sampler3D noiseTexture;
uniform sampler3D velocity;
uniform sampler3D dye;
uniform sampler1D lookUpTexture;
uniform sampler3D property;
uniform vec3 dyeTranslation;
uniform vec3 dyeScale;
uniform vec3 texCoordMult;
uniform vec3 deltaT;
uniform float time;
uniform float period;
uniform vec3 weightW;
uniform vec3 weightV;

void main(void)
{

   //look up the velocity in the field
   vec4 v = texture3D(velocity,gl_TexCoord[0]);

   //get our original values back
   v.xyz = (((v.xyz)*2.0) - 1.0);

   //velocity mask to darken slow moving flow
   float vMask = 1.0 - v.w;//sqrt(dot(v.xyz,v.xyz));

   //Euler integration
   //the old position
   vec3 oldTexCoord = gl_TexCoord[0].xyz + deltaT*v.xyz;
   
   //fetch the density using the old coord
   //density is our property that we are 
   //advecting
   vec4 prop = texture3D(property,oldTexCoord);
 
   //now for our materials
   
   //lookup the noise amplitude/phase
   vec4 n = texture3D(noiseTexture,gl_TexCoord[0].xyz*texCoordMult);

   //dye coordinate
   vec3 relFragCoord = (gl_TexCoord[0].xyz-dyeTranslation)*dyeScale; 
   float dyeAmp = texture3D(dye,relFragCoord).a;

   //now do the alpha blending for each material

   //material 1
   //get the local time
   gl_FragColor.x = weightW.x*prop.x + dyeAmp;

   //material 1
   float localTime = mod(time + n.y,period);
   float tInject = texture1D(lookUpTexture,localTime).a;
   gl_FragColor.y = weightW.y*prop.y + weightV.y*tInject*n.x;

   //material 2
   localTime = fmod(time + n.w,period);
   tInject = texture1D(lookUpTexture,localTime).a;
   gl_FragColor.z = weightW.z*prop.z + weightV.z*tInject*n.z;
   gl_FragColor.a = v.w;
}
