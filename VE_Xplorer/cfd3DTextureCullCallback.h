#ifndef CFD_3D_TEXTURE_UPDATE_CALLBACK_H
#define CFD_3D_TEXTURE_UPDATE_CALLBACK_H
#ifdef _OSG
#ifdef CFD_USE_SHADERS
#include <osg/Node>
#include <osg/NodeVisitor>
#include <osg/Texture3D>
#include <osgUtil/CullVisitor>
#include "cfdPBufferManager.h"
class cfdSliceNodeVisitor;
class cfd3DTextureCullCallback : public osg::NodeCallback
{
public:    
   cfd3DTextureCullCallback(osg::Node* subgraph,
		              osg::Texture3D* updateTexture,
			           cfdPBufferManager* pbm,
                   unsigned int nSlices);

   virtual void operator()(osg::Node* node, osg::NodeVisitor* nv);
   void preRender(osg::Node& node,osgUtil::CullVisitor& cv,int whichSlice);
        
   osg::Node* subgraph(){return _subgraph.get();}
        
protected:
   osg::ref_ptr<osg::Node> _subgraph;
   osg::ref_ptr<osg::Texture3D> _textureToUpdate;
   cfdPBufferManager* _pbuffer;
   osg::ref_ptr<osg::StateSet> _localState;
   unsigned int _nSlices;
};
#endif
#endif//_OSG
#endif //CFD_3D_TEXTURE_UPDATE_CALLBACK_H
