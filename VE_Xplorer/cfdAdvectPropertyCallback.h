#ifndef CFD_ADVECT_PROPERTY_CALLBACK_H
#define CFD_ADVECT_PROPERTY_CALLBACK_H

#ifdef _OSG
#include <osg/Node>
#include <osg/Texture3D>
#include <osgUtil/CullVisitor>

#include "cfdPBufferManager.h"

class cfdAdvectPropertyCallback : public osg::NodeCallback  
{
public:
   cfdAdvectPropertyCallback(osg::Node* subgraph);
   virtual ~cfdAdvectPropertyCallback();
   virtual void operator()(osg::Node* node, osg::NodeVisitor* nv);     
protected:
   osg::ref_ptr<osg::Node> _subgraph;
   
};

#endif //_OSG
#endif //CFD_ADVECT_PROPERTY_CALLBACK_H

