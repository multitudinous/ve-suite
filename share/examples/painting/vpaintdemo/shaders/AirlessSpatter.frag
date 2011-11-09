uniform sampler2D spatterTexture;
uniform float strength;

varying vec2 TexcoordLayerZero;

void main( void )
{
   vec4  fvBaseColor	= texture2D( spatterTexture, TexcoordLayerZero );
   float alpha			= fvBaseColor.a;
   float opacityOut		= alpha * strength;
   gl_FragColor			= vec4(opacityOut, opacityOut, opacityOut, opacityOut);
} // main
