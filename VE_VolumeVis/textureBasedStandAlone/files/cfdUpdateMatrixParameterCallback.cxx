#ifdef VE_PATENTED
#ifdef _OSG
#ifdef CFD_USE_SHADERS
#include <iostream>
#include <osgNV/MatrixParameterValue>
#include "cfdUpdateMatrixParameterCallback.h"

////////////////////////////////////////////////////////
//Constructors                                        //
////////////////////////////////////////////////////////
cfdUpdateMatrixParameterCallback::cfdUpdateMatrixParameterCallback() 
{ 
   _type = MATRIX;
   _size = FOUR;
   _scale[0] = 1.0;
   _scale[1] = 1.0;
   _scale[2] = 1.0;
   _translation[0] = 0.0;
   _translation[1] = 0.0;
   _translation[2] = 0.0;
   _center[0] = 0;
   _center[1] = 0;
   _center[2] = 0;
   _matrix = osg::Matrix::identity();
}
////////////////////////////////////////////////////////////////////////////////////
cfdUpdateMatrixParameterCallback
::cfdUpdateMatrixParameterCallback(const cfdUpdateMatrixParameterCallback &copy,
                          const osg::CopyOp &copyop )
:cfdUpdateParameterCallback(copy, copyop) 
{
   _matrix = osg::Matrix(copy._matrix);
}
///////////////////////////////////////////////////////////////////////////////
void cfdUpdateMatrixParameterCallback::operator()(osgNV::ParameterValue *param, 
                                            osg::State &state) const
{
   if(_type == MATRIX){
      osgNV::MatrixParameterValue *cgp = 
          dynamic_cast<osgNV::MatrixParameterValue *>(param);
      if (cgp){
         
         cgp->set(_matrix);
      }
   }
}
/////////////////////////////////////////////////////////////////////
void cfdUpdateMatrixParameterCallback::UpdateCenter(osg::Vec3 center)
{
   _center[0] = center.x();
   _center[1] = center.y();
   _center[2] = center.z();
   _updateMatrix();
}
//////////////////////////////////////////////////////////////
void cfdUpdateMatrixParameterCallback::UpdateScale(float* v)
{
   _scale[0] = v[0];
   _scale[1] = v[1];
   _scale[2] = v[2];
   
   _updateMatrix();
}
//////////////////////////////////////////////////////////////
void cfdUpdateMatrixParameterCallback::UpdateTranslation(float* v)
{
   _translation[0] = v[0];
   _translation[1] = v[1];
   _translation[2] = v[2];
   _updateMatrix();
}
//////////////////////////////////////////////////////
void cfdUpdateMatrixParameterCallback::_updateMatrix()
{
   //there has to be a better way of doing this
   osg::Matrix translate = osg::Matrix::translate(_translation[0],
                                             _translation[1],
                                             _translation[2]);
   osg::Matrix scale = osg::Matrix::scale(_scale[0],
                                      _scale[1],
                                      _scale[2]);
   osg::Matrix center = osg::Matrix::translate(-_center[0],
                                          -_center[1],
                                          -_center[2]);
   
   _matrix.set(scale*translate);
}
#endif //CFD_USE_SHADERS
#endif //_OSG
#endif
