//class to update the texture matrix appropriately
#ifndef CFD_TEXTURE_MATRIX_CALLBACK_H 
#define CFD_TEXTURE_MATRIX_CALLBACK_H 
#ifdef _OSG

namespace osg{
   class TexMat;
   class Node;
}
#include <osg/NodeCallback>
#include <osg/Vec3f>
class cfdTextureMatrixCallback : public osg::NodeCallback
{
public:
   cfdTextureMatrixCallback(osg::TexMat* texmat,osg::Vec3f center,
                       float* scale,float* trans);
    virtual void operator()(osg::Node* node,osg::NodeVisitor* nv);
    
protected:
   float _trans[3];
   float _scale[3];
   osg::Vec3f _center;
   mutable osg::ref_ptr<osg::TexMat> _texMat;
};
#endif //_OSG
#endif// CFD_TEXTURE_MATRIX_CALLBACK_H 
