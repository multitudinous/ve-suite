#ifndef CFD_3D_TEXTURE_UPDATE_CALLBACK_H
#define CFD_3D_TEXTURE_UPDATE_CALLBACK_H
#ifdef VE_PATENTED
#ifdef _OSG 
#ifdef CFD_USE_SHADERS
namespace osg{
   class Node;
   class Texture3D;
   class NodeVisitor;
   class Viewport;
   //class BoundingBox;
}
namespace osgUtil{
   class CullVisitor;
}
class cfdPBufferManager;


#include <osg/NodeCallback>
#include <osg/BoundingBox>
class cfd3DTextureCullCallback : public osg::NodeCallback
{
public:    
   cfd3DTextureCullCallback(osg::Node* subgraph,
                         osg::Texture3D* texture,
                         unsigned int width,
                         unsigned int height);

   virtual ~cfd3DTextureCullCallback();
   void SetPBuffer(cfdPBufferManager* pbuffer){_pbuffer = pbuffer;}
   virtual void operator()(osg::Node* node, osg::NodeVisitor* nv);

   void preRender(osg::Node& node,osgUtil::CullVisitor& cv);
        
   osg::Node* subgraph(){return _subgraph.get();}
        
protected:
   osg::ref_ptr<osg::Node> _subgraph;
   unsigned int _w;
   unsigned int _h;
   cfdPBufferManager* _pbuffer;
   osg::ref_ptr<osg::Texture3D> _textureToUpdate;
   
   osg::ref_ptr<osg::StateSet> _localState;
   
};
#endif
#endif//_OSG
#endif
#endif //CFD_3D_TEXTURE_UPDATE_CALLBACK_H
