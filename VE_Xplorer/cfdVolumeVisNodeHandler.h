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
class cfdVolumeVisNodeHandler{
public:
   cfdVolumeVisNodeHandler();
   cfdVolumeVisNodeHandler(const cfdVolumeVisNodeHandler& vvnh);
   virtual ~cfdVolumeVisNodeHandler();

   void SetBoundingBox(float* bbox);
   virtual void Init();
   void TurnOnBBox();
   void TurnOffBBox();
   void SetVolumeVizNode(osg::Group* vvn);
   osg::Group* GetVisualization();

   cfdVolumeVisNodeHandler& operator=(const cfdVolumeVisNodeHandler& vvnh);
protected:
   void _createVisualBBox();
   
   virtual void _attachVolumeVisNodeToGraph();
   
   osg::ref_ptr<osg::Switch>_bboxSwitch;
   osg::ref_ptr<osg::Group> _visualBoundingBox;
   osg::ref_ptr<osg::Group> _vvN;
   osg::ref_ptr<osg::Group> _topNode;
   osg::BoundingBox _bbox;
};

#endif //_OSG
#endif// CFD_VOLUME_VIZ_NODE_HANDLER_H
