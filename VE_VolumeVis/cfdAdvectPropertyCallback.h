#ifndef CFD_ADVECT_PROPERTY_CALLBACK_H
#define CFD_ADVECT_PROPERTY_CALLBACK_H
#ifdef VE_PATENTED
#ifdef _OSG 
#ifdef CFD_USE_SHADERS

namespace osg{
   class Node;
}

#include <osg/NodeCallback>

class cfd3DTextureCullCallback;

class cfdAdvectPropertyCallback : public osg::NodeCallback  
{
public:
   cfdAdvectPropertyCallback(osg::Node* subgraph);
   virtual ~cfdAdvectPropertyCallback();

   virtual void operator()(osg::Node* node, osg::NodeVisitor* nv);     
protected:
   osg::ref_ptr<osg::Node> _subgraph;
};
#endif //CFD_USE_SHADERS
#endif //_OSG
#endif
#endif //CFD_ADVECT_PROPERTY_CALLBACK_H

