#ifndef CFD_VOLUME_VIZ_NODE_HANDLER_H
#define CFD_VOLUME_VIZ_NODE_HANDLER_H

#ifdef _OSG
#include <osg/BoundingBox>
#include <osg/ref_ptr>
namespace osg
{
   class Group;
   class Switch;
   class TexGenNode;
}

class cfdTextureManager;
class cfdVolumeVisNodeHandler{
public:
   cfdVolumeVisNodeHandler();
   cfdVolumeVisNodeHandler(const cfdVolumeVisNodeHandler& vvnh);
   virtual ~cfdVolumeVisNodeHandler();

   void SetSwitchNode(osg::Switch* vvn);
   void SetAttachNode(osg::Group* attachNode);
   void SetCenter(osg::Vec3f center);
   void SetTextureScale(float* scale,bool isInverted = true);
   void SetTextureManager(cfdTextureManager* tm);
   void SetBoundingBox(float* bbox);
   void SetBoundingBoxName(char*name);
   void SetDecoratorName(char* name);
   bool IsThisActive();
   virtual void Init();
   
   void TurnOnBBox();
   void TurnOffBBox();

   void EnableDecorator();
   
   cfdVolumeVisNodeHandler& operator=(const cfdVolumeVisNodeHandler& vvnh);
protected:
   void _createVisualBBox();
   //set up the stateset for the decorator
   virtual void _setUpDecorator()=0;
   void _createTexGenNode();
   unsigned int _whichChildIsThis;
   cfdTextureManager* _tm;
   osg::ref_ptr<osg::Switch>_bboxSwitch;
   osg::ref_ptr<osg::Group> _visualBoundingBox;
   osg::ref_ptr<osg::Switch> _vvN;
   osg::ref_ptr<osg::Group> _decoratorGroup;
   osg::ref_ptr<osg::Group> _byPassNode;
   osg::ref_ptr<osg::TexGenNode> _texGenParams;
   osg::BoundingBox _bbox;
   osg::Vec3f _center;
   float _scale[3];
};

#endif //_OSG
#endif// CFD_VOLUME_VIZ_NODE_HANDLER_H
