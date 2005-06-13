#ifndef CFD_VOLUME_BILLBOARD_H
#define CFD_VOLUME_BILLBOARD_H
#ifdef _OSG
#include <osg/Matrix>
#include <osg/Vec3>
#include <osg/Billboard>
class cfdVolumeBillboard: public osg::Billboard{
public:
   cfdVolumeBillboard(){}
   virtual ~cfdVolumeBillboard(){}
   //override how the matrix is computed for the billboard
   virtual bool computeMatrix(osg::Matrix& modelview,
                    const osg::Vec3& eye_local,
                    const osg::Vec3& pos_local) const;
protected:
};
#endif //_OSG
#endif //CFD_VOLUME_BILLBOARD_H
