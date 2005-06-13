uniform sampler1D mat1Func; 
uniform sampler1D mat2Func;
uniform sampler1D mat3Func;
uniform sampler1D mat4Func;
uniform sampler3D densityTexture;
void main(void)
{
   //get the density for each material
   vec4 density = texture3D(densityTexture,gl_TexCoord[0]);
   
   vec4 red = texture1D(mat4Func,density.x);
   vec4 dye = vec4(red.x,0,0,red.a);

   vec4 ink = texture1D(mat1Func,density.y);
   //float4 mat1 = vec4(green.x,green.x,0,green.a);

   vec4 hole = texture1D(mat2Func,density.z);
   //vec4 mat2 = vec4(0,0,0,blue.a);
   ink = clamp((ink - hole),vec4(0,0,0,0),vec4(1,1,1,1));

   gl_FragColor = clamp(ink +dye,vec4(0,0,0,0),vec4(1,1,1,1));
   gl_FragColor.w *= .2;
   //gl_FragColor.w *= color.a;
}


