#ifndef CFD_AVDECTION_SUBGRAPH_H
#define CFD_AVDECTION_SUBGRAPH_H
#ifdef _OSG
#ifdef CFD_USE_SHADERS
class cfdTextureManager;
class cfdPBufferManager;

namespace osg{
   class Texture3D;
}
#include <osg/TexGenNode>
#include <osg/Drawable>

osg::ref_ptr<osg::TexGenNode> CreateAdvectionSubGraph(cfdTextureManager* tm,
                                                osg::Texture3D* updateTexture,
                                                cfdPBufferManager* pbm,
                                                osg::StateSet* stateset,
                                                float deltaZ);
///////////////////////////////////////////////////////////////
class pbufferDrawCallback :public osg::Drawable::DrawCallback{
public:  
   pbufferDrawCallback(osg::Texture3D* updateTexture,
                     cfdPBufferManager* pbm,
                     unsigned int nSlices,
                     float* center,
                     float radius);
   ~pbufferDrawCallback();
   void SetDimensions(unsigned int w,unsigned int h);
   void drawImplementation(osg::State&,const osg::Drawable*) const;
   
protected:
   osg::ref_ptr<osg::Texture3D> _texture;
   cfdPBufferManager* _pbuffer;
   float _deltaZ;
   float _radius;
   float _center[3];
   unsigned int _nSlices;
   unsigned int _w;
   unsigned int _h;
};
#endif// CFD_USE_SHADERS
#endif// _OSG
#endif //CFD_AVDECTION_SUBGRAPH_H
