#ifndef CFD_3D_TEXTURE_UPDATE_CALLBACK_H
#define CFD_3D_TEXTURE_UPDATE_CALLBACK_H
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
                         unsigned int width,
                         unsigned int height);/*,
		                    osg::Texture3D* updateTexture,
                         cfdPBufferManager* pbm,
                         osg::BoundingBox bbox,
                         float deltaZ,
                         unsigned int nSlices);*/

   virtual ~cfd3DTextureCullCallback();
   //void SetWhichSliceToUpdate(unsigned int sliceNumber);
   virtual void operator()(osg::Node* node, osg::NodeVisitor* nv);

   void preRender(osg::Node& node,osgUtil::CullVisitor& cv);//,int whichSlice);
        
   osg::Node* subgraph(){return _subgraph.get();}
        
protected:
   osg::ref_ptr<osg::Node> _subgraph;
   unsigned int _w;
   unsigned int _h;
   /*osg::ref_ptr<osg::Texture3D> _textureToUpdate;
   cfdPBufferManager* _pbuffer;
   unsigned int _nSlices;
   float _deltaZ;
   osg::ref_ptr<osg::Viewport> _viewport;
   osg::BoundingBox _bbox;*/
   //cfdCopyTo3DTextureStage* _update3DTexture;
   osg::ref_ptr<osg::StateSet> _localState;
   
};
#endif
#endif//_OSG
#endif //CFD_3D_TEXTURE_UPDATE_CALLBACK_H
