#ifndef CFD_AVDECTION_SUBGRAPH_H
#define CFD_AVDECTION_SUBGRAPH_H
#ifdef VE_PATENTED
#ifdef _OSG
#ifdef CFD_USE_SHADERS
class cfdOSGAdvectionShaderManager;
class cfdTextureManager;
class cfdPBufferManager;

namespace osg{
   class Texture3D;
}

#include <osg/Group>
#include <osg/Drawable>

osg::ref_ptr<osg::Group> CreateAdvectionSubGraph(cfdTextureManager* tm,
                                           cfdPBufferManager* pbm,
                                           float deltaZ);

#endif// CFD_USE_SHADERS
#endif// _OSG
#endif
#endif //CFD_AVDECTION_SUBGRAPH_H
