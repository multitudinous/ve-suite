#ifndef VE_SWITCH_H
#define VE_SWITCH_H
/*!\file Switch.h
Switch API
*/

/*!\class VE_SceneGraph::Switch
*
*/
#include "VE_Xplorer/SceneGraph/SceneNode.h"

#ifdef _PERFORMER
#include <Performer/pf/pfSwitch.h>
#elif _OSG
#include <osg/Switch>
#elif OPENSG
#endif

namespace VE_SceneGraph{
#ifdef _OSG
class VE_SCENEGRAPH_EXPORTS Switch : public osg::Switch, public SceneNode
#elif _PERFORMER
class VE_SCENEGRAPH_EXPORTS Switch : public pfSwitch, public SceneNode
#endif
{
public:
   Switch();
   Switch(const Switch& cSwitch);
   virtual ~Switch();

private:
   
};
}

#endif// VE_SWITCH_H
