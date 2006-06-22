#ifdef VE_PATENTED
#include "cfdInitializePbufferCallback.h"
////////////////////////////////////////////////////////////
cfdInitializePbufferCallback::cfdInitializePbufferCallback()
{
   _pbuffer = 0;
}
/////////////////////////////////////////////////////////////
cfdInitializePbufferCallback::~cfdInitializePbufferCallback()
{
   /*if(_pbuffer){
      delete _pbuffer;
   }*/
}
///////////////////////////////////////////////////////////////////////////////        
void cfdInitializePbufferCallback::operator()(osgProducer::OsgCameraGroup&, 
                                         osgProducer::OsgSceneHandler& sh, 
                                         const Producer::RenderSurface& )
{ 
   if (!_pbuffer)
   {
      _pbuffer = new cfdPBufferManager();
      _pbuffer->isSupported();
      
   }            

   // now safe to continue
   sh.init();
}
#endif