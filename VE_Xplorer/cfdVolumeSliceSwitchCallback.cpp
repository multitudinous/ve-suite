#include "cfdVolumeSliceSwitchCallback.h"
#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG
#include <osg/Geode>

#include <osgUtil/Cullvisitor>
////////////////////////////////////////////////////////////
//Constructor
////////////////////////////////////////////////////////////
cfdVolumeSliceSwitchCallback::cfdVolumeSliceSwitchCallback(
osg::Vec3 center)
:_center(center)
{
   _whichSlices = Z_POS;
   _lastDir = Z_POS;
}
///////////////////////////////////////////////////////////////////
void cfdVolumeSliceSwitchCallback::AddGeometrySlices(SliceDir dir,
		                   osg::ref_ptr<osg::Geometry> geom)
{
   if(!_switchSlices[dir].valid()){
      _switchSlices[dir] = new osg::Geometry; 
   }   
   _switchSlices[dir] = geom;
}
//////////////////////////////////////////////////////////////
void cfdVolumeSliceSwitchCallback::switchSlices(osg::Vec3 eye)
{
   osg::Vec3 xAxis(1,0,0);
   osg::Vec3 yAxis(0,1,0);
   osg::Vec3 zAxis(0,0,1);
   osg::Vec3 viewVector(0,0,0);

   viewVector = eye - _center;
   viewVector.normalize();
   float closeAxis = 0 ;
  
   closeAxis = _maximal(viewVector[0],
                      _maximal(viewVector[1],viewVector[2]));

   if(closeAxis == viewVector[0]){
      if(closeAxis <0)
         _whichSlices = X_NEG;
      else
         _whichSlices = X_POS;
      
   }else if(closeAxis == viewVector[1]){
      if(closeAxis <0)
         _whichSlices = Y_NEG;
      else
         _whichSlices = Y_POS;
      
   }else if(closeAxis == viewVector[2]){
      if(closeAxis <0)
         _whichSlices = Z_NEG;
      else
         _whichSlices = Z_POS;
      
   }

}
/////////////////////////////////////////////////////////////
float cfdVolumeSliceSwitchCallback::_maximal(float a,float b)
{
   return (fabs(a)>fabs(b))?a:b;
}
/////////////////////////////////////////////////////////////////
void cfdVolumeSliceSwitchCallback::operator()(osg::Node* node,
                                              osg::NodeVisitor* nv)
{
   osgUtil::CullVisitor* cullVisitor = dynamic_cast<osgUtil::CullVisitor*>(nv);
   if (cullVisitor ){
      osg::Vec3 viewVector = cullVisitor->getEyePoint();
      switchSlices(viewVector);
      if(_lastDir != _whichSlices){
         ((osg::Geode*)node)->setDrawable(0,_switchSlices[_whichSlices].get());
      }
   }
   _lastDir = _whichSlices;
   traverse(node,nv);
   
}
#endif

