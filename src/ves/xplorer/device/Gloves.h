/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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

#ifndef _VES_XPLORER_DEVICE_GLOVES_H
#define _VES_XPLORER_DEVICE_GLOVES_H

// --- VE-Suite Includes
#include <ves/VEConfig.h>

#include <ves/xplorer/device/Device.h>

#include <ves/xplorer/scenegraph/DCS.h>

#include <ves/open/xml/CommandPtr.h>

// --- VR Juggler Includes --- //
#include <gmtl/Vec.h>
#include <gmtl/Matrix.h>

#include <gadget/Type/PositionInterface.h>
#include <gadget/Type/DigitalInterface.h>
#include <gadget/Type/AnalogInterface.h>


// --- OSG Includes --- //
#include <osg/Geometry>
#include <osg/Matrix>

#include <osgbInteraction/HandNode.h>

#include <osgUtil/IntersectVisitor>

namespace osg
{
class Geode;
class Group;
class Vec4d;
class Vec3d;
class MatrixTransform;
class LineSegment;
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

/*!\file Gloves.h
 * Gloves API
 * \class ves::xplorer::Gloves
 * \namespace ves::xplorer
 *
 */
class VE_XPLORER_EXPORTS Gloves : public Device
{
public:
    ///Constructor
    Gloves();

    ///Destructor
    virtual ~Gloves();

    ///
    ///\return
    virtual Gloves* AsGloves();

    ///
    ///\param
    virtual void Enable( const bool& enable = true );

    ///Initialize some variables in the class
    virtual void Initialize();

    ///Processes keyboard events
    virtual void ProcessEvents( ves::open::xml::CommandPtr command );

    ///Set the rotation method
    ///\param input Indicates which rotation method is needed
    void SetHeadRotationFlag( int input );

    ///Identifies selection chosen by wand
    void SelectObject();

    ///Process if selection is valid
    ///\param listOfHits A vector containing CAD hit in selection process
    void ProcessHit( osgUtil::IntersectVisitor::HitList listOfHits );

    ///Update the events done by wand
    void UpdateObjectHandler();

    ///Set the start and end position
    ///\param startPoint The start position
    ///\param endPoint The end position
    void SetupStartEndPoint( osg::Vec3d* startPoint, osg::Vec3d* endPoint );

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
    void UpdateDeltaWandPosition();
    ///Get the plane equation constants normal to the wand in world space
    double* GetPlaneEquationConstantsNormalToWand();

    osg::Matrix CreateQuat( double* rotArray );
protected:
    ///Set the start and end point
    ///\param startPoint The start point
    ///\param endPoint The end point
    virtual void SetStartEndPoint( osg::Vec3d* startPoint, osg::Vec3d* endPoint );

    ///Draws a beam from the wand to object
    ///\param startPoint The start position
    ///\param endPoint The end position
    virtual void DrawLine( osg::Vec3d startPoint, osg::Vec3d endPoint );

    ///Roate about arbitrary axis
    void FreeRotateAboutWand( const bool freeRotate = true );

    ///Update the hand model with data from VR Juggler
    void UpdateHandModel();

    void UpdateRightHandGlove();
    void UpdateLeftHandGlove();

private:
    gadget::DigitalInterface digital[ 6 ]; ///Array handling button controls on wand
    int buttonData[ 6 ]; ///<do not know what this does

    gadget::PositionInterface wand; ///<VRJuggler's wand positional interface
    gadget::PositionInterface head; ///<VRJuggler's head positional interface
    gadget::PositionInterface mRightHandPos;
    gadget::PositionInterface mLeftHandPos;

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

    ves::open::xml::CommandPtr command; ///<Stores xml command

    double deltaTrans[ 3 ]; ///<Stores difference in translation from last position to to current

    osg::ref_ptr< osg::Geode > selectedGeometry; ///<Geometry currently selected
    double distance; ///<Used for scaling
    std::string laserName; ///<do not know what this does
    osg::Vec3d LastWandPosition; ///<Stores last wand position
    osg::ref_ptr< osg::Group > mRootNode; ///<do not know what this does

    osg::ref_ptr< osg::Geode > beamGeode;///<do not know what this does
    osg::ref_ptr< osg::Geometry > beamGeometry;///<do not know what this does
    osg::ref_ptr< osg::LineSegment > beamLineSegment;///<do not know what this does
    ///See if a button has been pushed
    bool m_buttonPushed;
    ///Quat used every frame to store and rotational increments
    osg::Quat m_rotIncrement;
    ///Array to hold work translation
    double m_worldTrans[ 3 ];
    ///A flag to control bullet debug info
    bool mDebugInfo;

    osg::ref_ptr< osgbInteraction::HandNode > mLeftHand;
    osg::ref_ptr< osgbInteraction::HandNode > mRightHand;

    gadget::AnalogInterface      mRightThumbMCP;
    gadget::AnalogInterface      mRightThumbPIP;

    gadget::AnalogInterface      mRightThumbIndexAbduction;

    gadget::AnalogInterface      mRightIndexMCP;
    gadget::AnalogInterface      mRightIndexPIP;

    gadget::AnalogInterface      mRightIndexMiddleAbduction;

    gadget::AnalogInterface      mRightMiddleMCP;
    gadget::AnalogInterface      mRightMiddlePIP;

    gadget::AnalogInterface      mRightMiddleRingAbduction;

    gadget::AnalogInterface      mRightRingMCP;
    gadget::AnalogInterface      mRightRingPIP;

    gadget::AnalogInterface      mRightRingPinkyAbduction;

    gadget::AnalogInterface      mRightPinkyMCP;
    gadget::AnalogInterface      mRightPinkyPIP;


    gadget::AnalogInterface      mLeftThumbMCP;
    gadget::AnalogInterface      mLeftThumbPIP;

    gadget::AnalogInterface      mLeftThumbIndexAbduction;

    gadget::AnalogInterface      mLeftIndexMCP;
    gadget::AnalogInterface      mLeftIndexPIP;

    gadget::AnalogInterface      mLeftIndexMiddleAbduction;

    gadget::AnalogInterface      mLeftMiddleMCP;
    gadget::AnalogInterface      mLeftMiddlePIP;

    gadget::AnalogInterface      mLeftMiddleRingAbduction;

    gadget::AnalogInterface      mLeftRingMCP;
    gadget::AnalogInterface      mLeftRingPIP;

    gadget::AnalogInterface      mLeftRingPinkyAbduction;

    gadget::AnalogInterface      mLeftPinkyMCP;
    gadget::AnalogInterface      mLeftPinkyPIP;
};
} //end device
} //end xplorer
} //end ves

#endif //GLOVES_H
