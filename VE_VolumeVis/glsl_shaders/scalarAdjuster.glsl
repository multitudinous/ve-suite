//a volume rendering shader which applies a 1D transfer function
uniform sampler3D volumeData; 
uniform sampler1D transferFunction;
void main(void)
{
   //dependent texture look up in transfer function
   float scalar = texture3D(volumeData,gl_TexCoord[0]).a;
   gl_FragColor = texture1D(transferFunction,scalar);

   //set the opacity to .2 for all fragments
   //gl_FragColor.a *= .2;
   gl_FragColor.a *= gl_Color.a;
}


