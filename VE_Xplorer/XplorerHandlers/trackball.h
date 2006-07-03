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
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef TRACKBALL_H
#define TRACKBALL_H

#include <gmtl/Matrix.h>

using namespace gmtl;

const double OneEightyDivPI=57.29577951;
const double PIDivOneEighty=.0174532925;

class Trackball{
public:
	Trackball();
	~Trackball();
	void Init();
	void Matrix();
	void Reshape(unsigned int width,unsigned int height);
	void SetFOVy(float _top,float _bottom,float _near);
	void Keyboard(int key);
	void Mouse(int button,int state,int x,int y);
	void Motion(int x,int y);

protected:
   bool tb_tracking;
   bool tb_moving;
   float tb_currPos[2];
   float tb_prevPos[2];
	float tb_aspectRatio;
   unsigned int tb_width;
   unsigned int tb_height;
	float tb_FOVy;
	int tb_key;
	int tb_button;
   float tb_angle;
   float tb_axis[3];
	float tb_max[3];
	float tb_min[3];
	Matrix44f tb_transform;
	Matrix44f tb_accuTransform;

	void ResetTransforms();
	void RotateView(float dx,float dy);
	void Twist(float dx,float dy);
	void Zoom(float dy);
	void Pan(float dx,float dy);
	void Rotate(float x,float y,float z,float angle);
};
#endif
