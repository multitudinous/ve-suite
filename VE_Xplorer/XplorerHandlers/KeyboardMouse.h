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
    KeyboardMouse();

    ///Destructor
    ~KeyboardMouse();

    ///Update the position in scene
    virtual void UpdateNavigation();

    ///Update the current object selected
    virtual void UpdateSelection();

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
    ///\param l
    ///\param r
    ///\param t
    ///\param b
    ///\param n
    ///\param f
    void SetFrustumValues( float l, float r, float t, float b, float n, float f );

    ///Bring all objects into view
    void FrameAll();

    ///Resets the scene to original position
    void ResetTransforms();

protected:
    ///Set the start and end point
    ///\param startPoint The start point
    ///\param endPoint The end point
    virtual void SetStartEndPoint( osg::Vec3f* startPoint, osg::Vec3f* endPoint );

    ///Draws a line to help visualize the selection process
    ///\param startPoint The start position
    ///\param endPoint The end position
    virtual void DrawLine( osg::Vec3f startPoint, osg::Vec3f endPoint );

private:
    ///Processes any keyboard events
    ///\param mode Determines whether in navigation mode or selection mode
    void ProcessKBEvents( int mode );

    ///Processes the navigation events
    void ProcessNavigationEvents();

    ///Processes the selection events
    void ProcessSelectionEvents();

    ///Process if selection is valid
    ///\param listOfHits A vector containing CAD hit in selection process
    void ProcessHit( osgUtil::IntersectVisitor::HitList listOfHits );

    ///Navigation functions called using the keyboard
    void NavKeyboard();

    ///Obtains mouse positions upon mouse presses and releases
    void NavMouse();

    ///Navigation using the mouse
    void NavMotion();

    ///Currently this does nothing
    void SelKeyboard();

    ///Calls ProcessSelectionEvents if valid
    void SelMouse();

    ///Currently this does nothing
    void SelMotion();

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
    ///\param x
    ///\param y
    ///\param z
    ///\param angle
    void Rotate( float x, float y, float z, float angle );

    bool m_animate;///<Determines whether in animation mode

    unsigned int m_width;///<Width of the window
    unsigned int m_height;///<Height of the window
      
    int m_key;///<Holds the value of the key being used
    int m_button;///<Holds the value of the mouse button being used
    int m_state;///<Determines if mouse button is pressed or released
    int m_x;///<x position of the mouse
    int m_y;///<y position of the mouse

    float m_aspectRatio;///<Aspect ratio of window
    float m_fovy;///<Field of view in the y direction
    float m_leftFrustum;///<The left frustum value
    float m_rightFrustum;///<The right frustum value
    float m_topFrustum;///<The top frustum value
    float m_bottomFrustum;///<The bottom frustum value
    float m_nearFrustum;///<The near frustum value
    float m_farFrustum;///<The far frustum value
    float m_currPos[2];///<The current mouse position
    float m_prevPos[2];///<The previous mouse position
    float m_magnitude;///<The magnitude of the mouse movement
    float m_sensitivity;///<

    double m_xminScreen;///<The minimum x position of the screen
    double m_xmaxScreen;///<The maximum x position of the screen
    double m_yminScreen;///<The minimum y position of the screen
    double m_ymaxScreen;///<The maximum y position of the screen
    double m_zvalScreen;///<The z position of the screen

    //In mData form     In row by column form
    //[ 0 4  8 12 ]     [ 00 01 02 03 ]
    //[ 1 5  9 13 ]     [ 10 11 12 13 ]
    //[ 2 6 10 14 ]     [ 20 21 22 23 ]
    //[ 3 7 11 15 ]     [ 30 31 32 33 ]
    gmtl::Matrix44f m_deltaTransform;///<The change to be applied to the current transform
    gmtl::Matrix44f m_currentTransform;///<The current transform matrix being manipulated

    osg::ref_ptr< osg::Geode > beamGeode;///<
    osg::ref_ptr< osg::Geode > selectedGeometry;///<The geometry being selected
    osg::ref_ptr< osg::LineSegment > beamLineSegment;///<

    gadget::KeyboardMouseInterface m_keyboard;///<VRJuggler's keyboard/mouse positional interface
    gadget::PositionInterface m_head;///<VRJuggler's head positional interface
};
}

#endif //KEYBOARD_MOUSE_H
