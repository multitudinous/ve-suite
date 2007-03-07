#ifndef DEVICE_H
#define DEVICE_H

/*!\file Device.h
Device API
*/
/*!\class VE_XPlorer::Device
* 
*/

#include "VE_Installer/include/VEConfig.h"

#ifdef _OSG
#include <osg/ref_ptr>

namespace osg
{
	class LineSegment;
}
#endif

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS Device
{
public:
   Device();
   virtual ~Device(){;}

	virtual void UpdateNavigation() = 0;
	virtual void UpdateSelection() = 0;

	//virtual osg::ref_ptr< osg::LineSegment > GetLineSegment() = 0;

private:			

};
}

#endif //DEVICE_H
