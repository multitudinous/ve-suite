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
 * Author:        $Author$
 * Id:            $Id$
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
// --- VE-Suite Stuff --- //
#include "VE_Installer/include/VEConfig.h"

#include "VE_Xplorer/XplorerHandlers/Device.h"

// --- VR Juggler Stuff --- //
#include <gadget/Type/KeyboardMouseInterface.h>
#include <gadget/Type/PositionInterface.h>

#include <boost/shared_ptr.hpp>

#include <gmtl/Matrix.h>

// --- OSG Stuff --- //
#include <osg/Geometry>

#include <osgUtil/IntersectVisitor>

// --- C/C++ Libraries --- //
#include <utility>

namespace osg 
{
   class Geode;
   class Group;
   class Vec4f;
   class Vec3f;
   class LineSegment;
}

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS KeyboardMouse : public Device
{
public:
	///Constructor
	KeyboardMouse( void );

	///Destructor
	~KeyboardMouse( void );

    ///Update the position in scene
	virtual void UpdateNavigation( void );

	///Update the current object selected
	virtual void UpdateSelection( void );
 
	///Do not know what this does
    float GetCPThreshold( void );

	///Sets the screen corner values
	///\param values A map of strings to doubles
    void SetScreenCornerValues( std::map< std::string, double > values );

	///Determines whether or not in animation mode
	///\param animate Bool to determine animation mode
	void Animate( bool animate );

	///Set the window properties
	///\param w Set the width of the window
	///\param h Set the height of the window
	void SetWindowValues( unsigned int w, unsigned int h );

	///Set the frustrum values
	///\param l Set the left side
	///\param r Set the right side
	///\param t Set the top side
	///\param b Set the bottom side
	///\param n Set the near plane
	///\param f Set the far plane
	void SetFrustumValues( float l, float r, float t, float b, float n, float f );

	///Resets the scene to original position
    void ResetTransforms( void );

	///Sets the scene so all objects are viewable
    void FrameAll( void );

protected:
    ///Set the start and end point
    ///\param startPoint The start point
    ///\param endPoint The end point
    virtual void SetStartEndPoint( osg::Vec3f* startPoint, osg::Vec3f* endPoint );

    ///Draws a 
    ///\param startPoint The start position
    ///\param endPoint The end position
    virtual void DrawLine( osg::Vec3f startPoint, osg::Vec3f endPoint );

private:
	///Processes any keyboard events
	///\param mode Determines whether in navigation mode or selection mode
    void ProcessKBEvents( int mode );

	///Processes the navigation events
    void ProcessNavigationEvents( void );

	///Processes the selection events
    void ProcessSelectionEvents( void );

    ///Process if selection is valid
    ///\param listOfHits A vector containing CAD hit in selection process
    void ProcessHit( osgUtil::IntersectVisitor::HitList listOfHits );

	///Navigation functions called using the keyboard
    void NavKeyboard( void );

	///Obtains mouse positions upon mouse presses and releases
	void NavMouse( void );

	///Navigation using the mouse
	void NavMotion( void );

	///Currently this does nothing
    void SelKeyboard( void );

	///Calls ProcessSelectionEvents if valid
	void SelMouse( void );

	///Currently this does nothing
	void SelMotion( void );

	///Do not know how to describe this
	///\param dx The change in the x direction
	///\param dy The change in the y direction
    void RotateView( float dx, float dy );

	///Do not know how to describe this
	///\param dx The change in the x direction
	///\param dy The change in the y direction	
	void Twist( float dx, float dy );

	///Handles movement in and out of the scene
	///\param dy The change in the y direction
	void Zoom( float dy );

	///Handles panning movements through the scene
	///\param dx The change in the x direction
	///\param dy The change in the y direction
	void Pan( float dx, float dy );

	///Handles rotation of the scene
	///\param x_val The x position
	///\param y_val The y position
	///\param z_val The z position
	///\param angle The angle
	void Rotate( float x_val, float y_val, float z_val, float angle );

    bool tb_animate; ///<Determines whether in animation mode
    bool tb_mode; ///<Currently not being used

    unsigned int width; ///<Width of the window
	unsigned int height; ///<Height of the window
      
    int key; ///<Holds the value of the key being used
	int button; ///<Holds the value of the mouse button being used
    int state; ///<Determines if mouse button is pressed or released
    int x; ///<x position
    int y; ///<y position

    float aspect_ratio; ///<Aspect ratio of window
	float fovy; ///<Do not know what this is
    float left; ///<Value of left side used in frustum calculation
    float right; ///<Value of right side used in frustum calculation
    float top; ///<Value of top side used in frustum calculation
    float bottom; ///<Value of bottom side used in frustum calculation
    float near_plane; ///<Value of near plane used in frustum calculation
    float far_plane; ///<Value of far plane used in frustum calculation
    float tb_currPos[2]; ///<Do not know what this is
	float tb_prevPos[2]; ///<Do not know what this is
	float tb_magnitude; ///<Do not know what this is
	float tb_sensitivity; ///<Do not know what this is
    float tb_threshold; ///<Do not know what this is
    float tb_jump; ///<Do not know what this is

    double wc_screen_xmin; ///<x minimum screen position
    double wc_screen_xmax; ///<x maximum screen position
    double wc_screen_ymin; ///<y minimum screen position
    double wc_screen_ymax; ///<y maximum screen position
    double wc_screen_zval; ///<Do not know what this is

    std::pair< double, double > screenRatios; ///<Do not know what this is

	//Is of form [row][column]
	gmtl::Matrix44f tb_transform; ///<Do not know what this is
    gmtl::Matrix44f tb_currTransform; ///<Do not know what this is
    gmtl::Matrix44f tb_worldTransform; ///<Do not know what this is
   
    osg::ref_ptr< osg::Geode > beamGeode; ///<Do not know what this is
    osg::ref_ptr< osg::Geode > selectedGeometry; ///<Do not know what this is
    osg::ref_ptr< osg::LineSegment > beamLineSegment; ///<Do not know what this is

    gadget::KeyboardMouseInterface mKeyboard; ///<VRJuggler's keyboard/mouse positional interface
    gadget::PositionInterface head; ///<VRJuggler's head positional interface
};
}

#endif //KEYBOARD_MOUSE_H
