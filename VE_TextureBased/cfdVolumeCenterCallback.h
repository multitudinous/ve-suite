//class to update the billboard center appropriately` 
#ifndef CFD_VOLUME_CENTER_CALLBACK_H 
#define CFD_VOLUME_CENTER_CALLBACK_H 
#ifdef VE_PATENTED
#ifdef _OSG

namespace osg
{
   class TexMat;
   class Node;
}
#include <osg/NodeCallback>
#include <osg/Vec3f>

#include "VE_Installer/include/VEConfig.h"

namespace VE_TextureBased
{
   class VE_TEXTURE_BASED_EXPORTS cfdVolumeCenterCallback : public osg::NodeCallback
   {
      public:
         cfdVolumeCenterCallback(osg::Vec3f center);
         virtual ~cfdVolumeCenterCallback(){}
         void Translate(float* translation); 
         virtual void operator()(osg::Node* node,osg::NodeVisitor* nv);
      protected:
         osg::Vec3f _center;
         osg::Vec3f _translate;
   };
}
#endif //_OSG
#endif
#endif// CFD_VOLUME_CENTER_CALLBACK_H 
