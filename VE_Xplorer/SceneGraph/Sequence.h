#ifndef VE_SEQUENCE_H
#define VE_SEQUENCE_H
/*!\file Sequence.h
Sequence API
*/

/*!\class VE_SceneGraph::Sequence
*
*/
#include "VE_Xplorer/SceneGraph/SceneNode.h"

#ifdef _PERFORMER
#//include <Performer/pf/pfSequence.h>
#elif _OSG
#include <osg/Sequence>
#elif OPENSG
#endif

namespace VE_SceneGraph{
#ifdef _PERFORMER
class VE_SCENEGRAPH_EXPORTS Sequence : 
#elif _OSG
class VE_SCENEGRAPH_EXPORTS Sequence : public osg::Sequence, public SceneNode
#endif
{
      public:
         Sequence();
         virtual ~Sequence();

};
}

#endif //VE_SEQUENCE_H
