#ifndef CFD_VOLUME_VIZ_NODE_HANDLER_H
#define CFD_VOLUME_VIZ_NODE_HANDLER_H

#ifdef _OSG
#include <osg/BoundingBox>
#include <osg/ref_ptr>
namespace osg
{
   class Group;
   class Switch;
}

class cfdTextureManager;
class cfdVolumeVisNodeHandler{
public:
   cfdVolumeVisNodeHandler();
   cfdVolumeVisNodeHandler(const cfdVolumeVisNodeHandler& vvnh);
   virtual ~cfdVolumeVisNodeHandler();

   void SetSwitchNode(osg::Switch* vvn);
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
   unsigned int _whichChildIsThis;
   cfdTextureManager* _tm;
   osg::ref_ptr<osg::Switch>_bboxSwitch;
   osg::ref_ptr<osg::Group> _visualBoundingBox;
   osg::ref_ptr<osg::Switch> _vvN;
   osg::ref_ptr<osg::Group> _decoratorGroup;
   osg::ref_ptr<osg::Group> _byPassNode;
   osg::BoundingBox _bbox;
};

#endif //_OSG
#endif// CFD_VOLUME_VIZ_NODE_HANDLER_H
