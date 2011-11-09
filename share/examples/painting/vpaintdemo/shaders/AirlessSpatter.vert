varying vec2 TexcoordLayerZero;

void main( void )
{
   gl_Position = ftransform();
   TexcoordLayerZero    = gl_MultiTexCoord0.xy;
}