#ifdef _OSG
#include "cfdVolumeBillboard.h"

bool cfdVolumeBillboard::computeMatrix(osg::Matrix& modelview,
                                   const osg::Vec3& eye_local,
                                   const osg::Vec3& pos_local) const
{
   return osg::Billboard::computeMatrix(modelview,eye_local,pos_local);
   //osg::Billboard::computeMatrix(modelview,eye_local,pos_local);
   osg::Matrix matrix;
   osg::Vec3 ev(eye_local-pos_local);
   osg::Vec3f eyeTemp; 
   osg::Vec3f center;
   osg::Vec3f up;
   modelview.getLookAt(eyeTemp, center, up);
   //billboards matrix is basically looking at 
   //the camera from the rotation center
   matrix.makeLookAt(_positionList[0],eyeTemp,up);
   
   //matrix.setTrans(pos_local);
   modelview.preMult(matrix);
   return true;
   
}
#endif
