//class to update the texture matrix appropriately
#ifdef VE_PATENTED
#ifdef _OSG
#include "cfdTextureMatrixCallback.h"
#include <osg/TexMat>
#include <osg/Matrix>
#include <osg/Node>
////////////////////////////////////////////////////////////////////////
//Constructor                                                         //
////////////////////////////////////////////////////////////////////////
cfdTextureMatrixCallback::cfdTextureMatrixCallback(osg::TexMat* texmat,
                                             osg::Vec3f center,
                                        float* scale,float* trans)
:_texMat(texmat),_center(center)
{
   _scale[0] = scale[0];
   _scale[1] = scale[1];
   _scale[2] = scale[2];

   _trans[0] = trans[0];
   _trans[1] = trans[1];
   _trans[2] = trans[2];
}
////////////////////////////////////////////////////////////////////////////
void cfdTextureMatrixCallback::operator()(osg::Node* node,osg::NodeVisitor* nv)
{
   //osgUtil::CullVisitor* cv = dynamic_cast<osgUtil::CullVisitor*>(nv);
   if (_texMat.valid()){
      osg::Matrix translate = osg::Matrix::translate(_trans[0],_trans[1],_trans[2]);
      osg::Matrix scale = osg::Matrix::scale(_scale[0],_scale[1],_scale[2]);
      osg::Matrix center = osg::Matrix::translate(-_center[0],-_center[1],-_center[2]);
      
      _texMat->setMatrix(center*scale*translate);
   }
   traverse(node,nv);
}
#endif //_OSG
#endif
