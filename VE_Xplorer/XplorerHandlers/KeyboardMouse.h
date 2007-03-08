/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date:  $
 * Version:       $Rev:  $
 * Author:        $Author:  $
 * Id:            $Id:  $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef KEYBOARD_MOUSE_H
#define KEYBOARD_MOUSE_H
/*!\file KeyboardMouse.h
KeyboardMouse API
*/
/*!\class VE_XPlorer::KeyboardMouse
* 
*/
#include "VE_Installer/include/VEConfig.h"

#include <boost/shared_ptr.hpp>
#include <gadget/Type/KeyboardMouseInterface.h>

#include <gmtl/Matrix.h>

#include "VE_Xplorer/XplorerHandlers/Device.h"

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS KeyboardMouse : public Device
{
public:
	KeyboardMouse();
	~KeyboardMouse();

	virtual void UpdateNavigation();
	virtual void UpdateSelection();

	void Animate( bool animate );
	void Reshape( unsigned int width, unsigned int height );
	void SetFOVy( float _top, float _bottom, float _near );
   void ResetTransforms();
   void FrameAll();

private:
   gadget::KeyboardMouseInterface mKeyboard;

   void TBKeyboard( int key );
	void TBMouse( int button, int state, int x, int y );
	void TBMotion( int x, int y );

	bool tb_moving;
	bool tb_animate;

	unsigned int tb_width;
	unsigned int tb_height;

	int tb_key;
	int tb_button;

	float tb_currPos[2];
	float tb_prevPos[2];
	float tb_magnitude;
	float tb_sensitivity;
	float tb_aspectRatio;
	float tb_FOVyRatio;
	float tb_FOVy;

	//Is of form [row][column]
	gmtl::Matrix44f tb_transform;
	gmtl::Matrix44f tb_accuTransform;

	void RotateView( float dx, float dy );
	void Twist( float dx, float dy );
	void Zoom( float dy );
	void Pan( float dx, float dy );
	void Rotate( float x, float y, float z, float angle );
};
}

#endif //KEYBOARD_MOUSE_H
