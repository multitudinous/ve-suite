#ifndef CFD_3D_TEXTURE_UPDATE_CALLBACK_H
#define CFD_3D_TEXTURE_UPDATE_CALLBACK_H
#ifdef _OSG
#include <osg/Node>
#include <osg/NodeVisitor>
#include <osg/Texture3D>

class cfd3DTextureCullCallback : public osg::NodeCallback
{
public:    
   cfd3DTextureUpdateCallback(osg::Node* subgraph,
		              osg::Texture3D* updateTexture,
			      cfdPBufferManager* pbm);

   virtual void operator()(osg::Node* node, osg::NodeVisitor* nv);
   void preRender(osg::Node& node,osgUtil::Cullvisitor* cv);
        
   osg::Node* subgraph(){return _subgraph.get();}
        
protected:
   osg::ref_ptr<osg::Node> _subgraph;
   osg::ref_ptr<osg::Texture3D> _textureToUpdate;
   cfdPBufferManager* _pbuffer;
};
#endif//_OSG
#endif //CFD_3D_TEXTURE_UPDATE_CALLBACK_H
