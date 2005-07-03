#ifndef CFD_AVDECTION_SUBGRAPH_H
#define CFD_AVDECTION_SUBGRAPH_H
#ifdef VE_PATENTED
#ifdef _OSG
namespace VE_TextureBased
{
   class cfdOSGAdvectionShaderManager;
   class cfdTextureManager;
   class cfdPBufferManager;
}

namespace osg
{
   class Texture3D;
}

#include <osg/Group>
#include <osg/Drawable>
#include "VE_Installer/include/VEConfig.h"

namespace VE_TextureBased
{
   VE_TEXTURE_BASED_EXPORTS osg::ref_ptr<osg::Group> CreateAdvectionSubGraph(cfdTextureManager* tm,
                                           cfdPBufferManager* pbm,
                                           float deltaZ);

}
#endif// _OSG
#endif
#endif //CFD_AVDECTION_SUBGRAPH_H
