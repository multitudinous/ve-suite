#ifndef CFD_AVDECTION_SUBGRAPH_H
#define CFD_AVDECTION_SUBGRAPH_H
#ifdef _OSG
#ifdef CFD_USE_SHADERS
class cfdTextureManager;
class cfdPBufferManager;

namespace osg{
   class Texture3D;
   class StateSet;
}
namespace osgNVCg{
   class Program;
}
#include <osg/Group>
#include <osg/Drawable>

osg::ref_ptr<osg::Group> CreateAdvectionSubGraph(cfdTextureManager* tm,
                                                osg::Texture3D* updateTexture,
                                                cfdPBufferManager* pbm,
                                                osg::StateSet* stateset,
                                                osgNVCg::Program* vertpg,
                                                float deltaZ);

#endif// CFD_USE_SHADERS
#endif// _OSG
#endif //CFD_AVDECTION_SUBGRAPH_H
