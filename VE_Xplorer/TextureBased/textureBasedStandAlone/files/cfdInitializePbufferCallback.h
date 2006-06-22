#ifndef CFD_INITIALIZE_PBUFFER_CALLBACK_H
#define  CFD_INITIALIZE_PBUFFER_CALLBACK_H
#ifdef VE_PATENTED
#include <osgProducer/Viewer>
#include "cfdPBufferManager.h"

class cfdInitializePbufferCallback : public osgProducer::OsgCameraGroup::RealizeCallback
{
public:
   cfdInitializePbufferCallback();
   virtual ~cfdInitializePbufferCallback();
   
   virtual void operator()(osgProducer::OsgCameraGroup&, 
                         osgProducer::OsgSceneHandler& sh, 
                         const Producer::RenderSurface& );
   
   cfdPBufferManager* pbuffer(){return _pbuffer;}
protected:
   
   cfdPBufferManager* _pbuffer;
        
};
#endif
#endif //CFD_INITIALIZE_PBUFFER_CALLBACK_H