#ifndef CFD_UPDATE_MATRIX_PARAMETER_CALLBACK_H
#define CFD_UPDATE_MATRIX_PARAMETER_CALLBACK_H
#ifdef VE_PATENTED
#ifdef _OSG
#ifdef CFD_USE_SHADERS
#include <osgNV/Version>
#include "cfdUpdateParameterCallback.h"
#include <osg/Matrix>

class cfdUpdateMatrixParameterCallback:public cfdUpdateParameterCallback
{
public:
   cfdUpdateMatrixParameterCallback(); 
   cfdUpdateMatrixParameterCallback(const cfdUpdateMatrixParameterCallback &copy, const osg::CopyOp &copyop = osg::CopyOp::SHALLOW_COPY); 

   META_Object(test, cfdUpdateMatrixParameterCallback);
   void operator()(osgNV::ParameterValue *param, osg::State &state) const;
	
   void UpdateScale(float* value);
   void UpdateTranslation(float* value);
   void UpdateCenter(osg::Vec3 center);
   
protected:
   void _updateMatrix();
   float _scale[3];
   float _translation[3];
   float _center[3];
   osg::Matrix _matrix;
};
#endif //CFD_USE_SHADERS
#endif //_OSG
#endif //CFD_UPDATE_PARAMETER_CALLBACK_H
#endif
