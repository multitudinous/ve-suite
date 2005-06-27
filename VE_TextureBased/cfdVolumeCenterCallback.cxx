//class to update the billboard center appropriately` 
#ifdef VE_PATENTED
#ifdef _OSG
#include <osg/Billboard>
#include <osg/NodeCallback>
#include <osg/Vec3f>
#include "VE_TextureBased/cfdVolumeCenterCallback.h"
using namespace VE_TextureBased;
///////////////////////////////////////////////////////////////////
//Constructor                                                    //
///////////////////////////////////////////////////////////////////
cfdVolumeCenterCallback::cfdVolumeCenterCallback(osg::Vec3f center)
:_center(center)
{
   _translate[0] = 0;
   _translate[1] = 0;
   _translate[2] = 0;
}
///////////////////////////////////////////////////////////
void cfdVolumeCenterCallback::Translate(float* translation)
{
   _translate[0] = translation[0];
   _translate[1] = translation[2];
   _translate[2] = translation[2];
}
//////////////////////////////////////////////////////////////////////////////
void cfdVolumeCenterCallback::operator()(osg::Node* node,osg::NodeVisitor* nv)
{
   osg::ref_ptr<osg::Billboard> billboard = dynamic_cast<osg::Billboard*>(node);
   if(billboard.valid())
   {
      billboard->setPosition(0,_center + _translate);
   }
   traverse(node,nv);
}
#endif //_OSG
#endif
