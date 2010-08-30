/*************** <auto-copyright.rb BEGIN do not edit this line> **************
*
* VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
*************** <auto-copyright.rb END do not edit this line> ***************/

#ifndef WAND_H
#define WAND_H

// --- VE-Suite Includes
#include <ves/VEConfig.h>

#include <ves/xplorer/device/Device.h>

#include <ves/xplorer/scenegraph/DCS.h>

#include <ves/open/xml/CommandPtr.h>

#ifdef QT_ON
#include <boost/signals2/signal.hpp>
#include <ves/xplorer/eventmanager/InteractionEvent.h>
#endif // QT_ON

// --- VR Juggler Includes --- //
#include <gmtl/Vec.h>
#include <gmtl/Matrix.h>
#include <gadget/Type/PositionInterface.h>
#include <gadget/Type/DigitalInterface.h>

// --- OSG Includes --- //
#include <osg/Geometry>
#include <osg/MatrixTransform>
#include <osg/Depth>

#include <osgUtil/IntersectVisitor>

namespace osg
{
class Geode;
class Group;
class Vec4d;
class Vec3d;
}

namespace osgUtil
{
class LineSegmentIntersector;
}

namespace ves
{
namespace xplorer
{

namespace scenegraph
{
class DCS;
}

namespace device
{
/*!\file Wand.h
 * Wand API
 */

/*!\class ves::xplorer::Wand
 *
 */

/*!\namespace ves::xplorer
 *
 */
class VE_XPLORER_EXPORTS Wand : public Device
{
public:
    ///Constructor
    Wand();

    ///Destructor
    virtual ~Wand();

    ///
    ///\return
    virtual Wand* AsWand();

    ///Initialize some variables in the class
    virtual void Initialize();

    ///Processes wand events
    virtual void ProcessEvents( ves::open::xml::CommandPtr command );

    ///Set the rotation method
    ///\param input Indicates which rotation method is needed
    void SetHeadRotationFlag( int input );

    ///Does not let the user go below the ground plane at 0,0,0
    ///\param input Flag to insure translation does not go below zero plane
    void SetSubZeroFlag( int input );

    ///Process if selection is valid
    void ProcessHit();

    ///Update the events done by wand
    void UpdateObjectHandler();

    ///Performs translation through scene
    void TranslateObject();

    //osg::MatrixTransform* getMatrixTransform( void );

    ///Get the current direction of the wand
    double* GetDirection();

    ///Get the current object location
    double* GetObjLocation();

    ///Transform direction from VRJuggler to OSG
    void UpdateWandLocalDirection();

    ///Transform wand point into global space
    void UpdateWandGlobalLocation();

    ///Find translation difference from last position to current
    //void UpdateDeltaWandPosition();
    ///Get the plane equation constants normal to the wand in world space
    double* GetPlaneEquationConstantsNormalToWand();

protected:
    ///Set the start and end point
    ///\param startPoint The start point
    ///\param endPoint The end point
    virtual void SetStartEndPoint( osg::Vec3d* startPoint, osg::Vec3d* endPoint );

    ///Set the start and end position
    ///\param startPoint The start position
    ///\param endPoint The end position
    void SetupStartEndPoint( osg::Vec3d& startPoint, osg::Vec3d& endPoint );

    ///Draws a beam from the wand to object
    ///\param startPoint The start position
    ///\param endPoint The end position
    virtual void DrawLine( const osg::Vec3d& startPoint, const osg::Vec3d& endPoint );

    ///Identifies selection chosen by wand
    void UpdateSelectionLine( bool drawLine = true );
    
    ///Make the raw geometry for the wand line
    void MakeWandLine();

    ///Roate about arbitrary axis
    void FreeRotateAboutWand( const bool freeRotate = true );

private:
    gadget::DigitalInterface digital[ 6 ]; ///Array handling button controls on wand
    int buttonData[ 6 ]; ///<do not know what this does

    int cfdIso_value; ///<Value to translate

    gadget::PositionInterface wand; ///<VRJuggler's wand positional interface
    gadget::PositionInterface head; ///<VRJuggler's head positional interface

    gmtl::Matrix44d vjHeadMat; ///<Contains current head position matrix

    double dir[ 3 ]; ///<Direction of the wand
    double worldLoc[ 3 ]; ///<Location of the objects with respect to the virtual space
    double cursorLoc[ 3 ]; ///<Location of the cursor with respect to the virtual space
    double objLoc[ 3 ]; ///<Location with respect to data set (the actual location to interact with data
    double cursorLen; ///<Cursor length

    double translationStepSize; ///<Size of translation step
    double rotationStepSize; ///<Size of rotation step
    ///Constants for the plane normal to the wand in world space
    double m_planeConstants[ 4 ];
    int rotationFlag; ///<Rotation flag
    int subzeroFlag; ///<Zero plane flag

    double deltaTrans[ 3 ]; ///<Stores difference in translation from last position to to current

    osg::ref_ptr< osg::Geode > selectedGeometry; ///<Geometry currently selected
    double m_distance; ///<Used for scaling
    osg::Vec3d LastWandPosition; ///<Stores last wand position
    osg::Node* rootNode; ///<do not know what this does

    ///do not know what this does
    osg::ref_ptr< osgUtil::LineSegmentIntersector > m_beamLineSegment;
    ///Transform to manipulate the selection line
    osg::ref_ptr< osg::MatrixTransform > m_wandPAT;
    ///Depth attribute 
    osg::ref_ptr< osg::Depth > m_depth;
    
    ///See if a button has been pushed
    bool m_buttonPushed;
    ///Quat used every frame to store and rotational increments
    osg::Quat m_rotIncrement;
    ///Array to hold work translation
    double m_worldTrans[ 3 ];

#ifdef QT_ON
    typedef boost::signals2::signal<void (ves::xplorer::eventmanager::InteractionEvent&)> InteractionSignal_type;
    InteractionSignal_type m_interactionSignal;
#endif // QT_ON
    
};
} //end device
} //end xplorer
} //end ves

#endif //WAND_H
