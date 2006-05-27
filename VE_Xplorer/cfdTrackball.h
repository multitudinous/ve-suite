#ifndef CFD_TRACKBALL_H
#define CFD_TRACKBALL_H

#include <boost/shared_ptr.hpp>
#include <gadget/Type/KeyboardMouseInterface.h>
#include <gadget/Type/KeyboardMouse/KeyEvent.h>
#include <gadget/Type/KeyboardMouse/MouseEvent.h>


#include "VE_Xplorer/trackball.h"

#include "VE_Installer/include/VEConfig.h"

using namespace boost;

namespace VE_Xplorer
{
	class VE_XPLORER_EXPORTS cfdTrackball
	{
	public:
		cfdTrackball();
		~cfdTrackball();
		void preFrame();
		void Matrix();
		void Reshape(unsigned int width,unsigned int height);
	private:
		gadget::KeyboardMouseInterface mKeyboard;
		Trackball tb;
	};
}

#endif