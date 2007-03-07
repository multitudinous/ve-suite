#ifndef WAND_H
#define WAND_H

/*!\file Wand.h
Wand API
*/
/*!\class VE_XPlorer::Wand
* 
*/

#include "VE_Installer/include/VEConfig.h"

#include "VE_Xplorer/XplorerHandlers/Device.h"

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS Wand : public Device
{
public:
	Wand();
	~Wand();

	virtual void UpdateNavigation();
	virtual void UpdateSelection();

private:

};
}

#endif //WAND_H