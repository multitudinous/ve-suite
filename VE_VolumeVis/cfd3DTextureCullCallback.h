#ifndef CFD_3D_TEXTURE_UPDATE_CALLBACK_H
#define CFD_3D_TEXTURE_UPDATE_CALLBACK_H
#ifdef VE_PATENTED
#ifdef _OSG
//#ifdef CFD_USE_SHADERS
namespace osg{
   class Node;
   class Texture3D;
   class NodeVisitor;
   class Viewport;
   class FrameStamp;
   //class BoundingBox;
}
namespace osgUtil{
   class CullVisitor;
   class UpdateVisitor;
}
class cfdPBufferManager;
class cfdOSGPingPongTexture3D;

#include <osg/Node>
#include <osg/NodeCallback>
#include <osg/BoundingBox>
class cfd3DTextureCullCallback : public osg::NodeCallback
{
public:    
   cfd3DTextureCullCallback(osg::Node* subgraph,
                         //osg::Texture3D* texture,
                         unsigned int width,
                         unsigned int height);

   virtual ~cfd3DTextureCullCallback();
   void SetPBuffer(cfdPBufferManager* pbuffer){_pbuffer = pbuffer;}
   void SetPingPongTextures(unsigned int tPingUint,
                         osg::Node* ping,
                         unsigned int tPongUint, 
                         osg::Node* pong);
   virtual void operator()(osg::Node* node, osg::NodeVisitor* nv);

   void preRender(osg::Node& node,osgUtil::CullVisitor& cv);
   cfdOSGPingPongTexture3D* GetPingPonger();
   osg::Node* subgraph(){return _subgraph.get();}
        
protected:
   osg::ref_ptr<osg::Node> _subgraph;
   unsigned int _w;
   unsigned int _h;
   unsigned int _count;
   cfdPBufferManager* _pbuffer;
   cfdOSGPingPongTexture3D* _pingPonger;  
   osg::ref_ptr<osg::Texture3D> _textureToUpdate;
   osg::ref_ptr<osg::Node> _current;
   osg::ref_ptr<osg::Node> _previous;
   osg::ref_ptr<osg::StateSet> _localState;
   osgUtil::UpdateVisitor* _uniformUpdater;
   osg::ref_ptr<osg::FrameStamp> _fs;

   
};

#endif//_OSG
#endif
#endif //CFD_3D_TEXTURE_UPDATE_CALLBACK_H
