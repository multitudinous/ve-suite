#ifndef CFD_VOLUME_SLICE_SWITCH_CALLBACK_H
#define CFD_VOLUME_SLICE_SWITCH_CALLBACK_H

#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG
#include <osg/Node>
#include <osg/Geometry>
#include <osg/Vec3>

class cfdVolumeSliceSwitchCallback : public osg::NodeCallback
{
public:
   cfdVolumeSliceSwitchCallback(osg::Vec3 center);
   enum SliceDir{X_POS=0,Y_POS, Z_POS,X_NEG,Y_NEG ,Z_NEG};
   void AddGeometrySlices(SliceDir direction,osg::ref_ptr<osg::Geometry> geom);
   void switchSlices(osg::Vec3 eye);
   virtual void operator()(osg::Node* node, osg::NodeVisitor* nv);
        
protected:
   osg::Vec3 _center;
   float _maximal(float a,float b);
   virtual ~cfdVolumeSliceSwitchCallback(){}
   osg::ref_ptr<osg::Geometry> _switchSlices[6];
   SliceDir _whichSlices;
   SliceDir _lastDir;
};
#endif
#endif// CFD_VOLUME_SLICE_SWITCH_CALLBACK_H

