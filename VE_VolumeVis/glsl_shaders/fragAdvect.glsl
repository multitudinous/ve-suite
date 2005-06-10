struct frag2app{
   float4 color : COLOR;
};

frag2app fp_advectTexture(float3 tCoords:TEXCOORD0,
                          float4 color : COLOR,
                    uniform sampler3D noiseTexture:TEXUNIT0,
                    uniform sampler3D velocity:TEXUNIT1,
                    uniform sampler3D dye:TEXUNIT2,
                    uniform sampler1D lookUpTexture:TEXUNIT3,
                    uniform sampler3D property:TEXUNIT4,
                    uniform float3 dyeTranslation,
                    uniform float3 dyeScale,
                    uniform float3 texCoordMult,
                    uniform float3 deltaT,
                    uniform float time,
                    uniform float period,
                    uniform float4 weightW,
                    uniform float4 weightV
                    )
{
   frag2app retColor;

   //look up the velocity in the field
   //float4 v = float4(.5,.5,0,0);//tex3D(velocity,tCoords);
   float4 v = tex3D(velocity,tCoords);

   //get our original values back
   v.xyz = (((v.xyz)*2.0) - 1.0);

   //velocity mask to darken slow moving flow
   float vMask = 1.0 - v.w;//sqrt(dot(v.xyz,v.xyz));

   //Euler integration
   //the old position
   float3 oldTexCoord = tCoords.xyz + deltaT*v.xyz;
   
   //fetch the density using the old coord
   //density is our property that we are 
   //advecting
   float4 prop = tex3D(property,oldTexCoord);
 
   //now for our materials
   
   //lookup the noise amplitude/phase
   float4 n = tex3D(noiseTexture,tCoords.xyz*texCoordMult);

   //dye coordinate
   float3 relFragCoord = (tCoords-dyeTranslation)*dyeScale; 
   float dyeAmp = tex3D(dye,relFragCoord).a;

   //now do the alpha blending for each material

   //material 1
   //get the local time
   retColor.color.x = weightW.x*prop.x + dyeAmp;

   //material 1
   float localTime = fmod(time + n.y,period);
   float tInject = tex1D(lookUpTexture,localTime).a;
   retColor.color.y = weightW.y*prop.y + weightV.y*tInject*n.x;

   //material 2
   localTime = fmod(time + n.w,period);
   tInject = tex1D(lookUpTexture,localTime).a;
   retColor.color.z = weightW.z*prop.z + weightV.z*tInject*n.z;
   retColor.color.a = v.w;
   return retColor;
}
