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

// --- VE-Suite Includes --- //
#include <ves/xplorer/device/Pointer.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <ves/xplorer/Debug.h>
#include <ves/xplorer/DeviceHandler.h>

#include <ves/xplorer/environment/cfdEnum.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/FindParentsVisitor.h>
#include <ves/xplorer/scenegraph/util/NormalizeVisitor.h>

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>

// --- vrJuggler Includes --- //
#include <gmtl/Xforms.h>
#include <gmtl/Generate.h>
#include <gmtl/Misc/MatrixConvert.h>

// --- OSG Includes --- //
#include <osg/LineSegment>
#include <osg/Group>
#include <osg/Geometry>
#include <osg/MatrixTransform>
#include <osg/Array>
#include <osg/NodeVisitor>
#include <osgDB/ReadFile>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace gmtl;
using namespace gadget;
using namespace ves::xplorer::device;
using namespace ves::xplorer::scenegraph;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////////////////
Pointer::Pointer()
    :
    Device( POINTER ),
    cursorLen( 1.0f ),
    translationStepSize( 0.75f ),
    rotationStepSize( 1.0f ),
    rotationFlag( 1 ),
    distance( 1000 ),
    m_buttonPushed( false ),
    mDebugInfo( false )
{
    command = ves::open::xml::CommandPtr();
    wand.init( "VJWand" );
    head.init( "VJHead" );
    m_pointerPosition.init( "VESPointer" );

    beamLineSegment = new osg::LineSegment();

    mRootNode = ves::xplorer::DeviceHandler::instance()->GetDeviceGroup();

    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
Pointer* Pointer::AsPointer()
{
    return this;
}
////////////////////////////////////////////////////////////////////////////////
void Pointer::Enable( const bool& enable )
{
    m_enabled = enable;

    if( m_enabled )
    {
        mRootNode->addChild( m_pointerDCS.get() );
    }
    else
    {
        if( mRootNode->containsNode( m_pointerDCS.get() ) )
        {
            mRootNode->removeChild( m_pointerDCS.get() );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void Pointer::Initialize()
{
    for( int i = 0; i < 3; ++i )
    {
        cursorLoc[ i ] = 0;
        //loc[ i ] + dir[ i ] * cursorLen;
        objLoc[ i ] = cursorLoc[ i ];
    }

    osg::Vec3 pos;
    osg::Quat quat;

    /////////
    osg::ref_ptr< osg::Node > handFile = osgDB::readNodeFile( "osg-data/arrowmono.ive" );

    if( !handFile.valid() )
    {
        osg::notify( osg::FATAL ) << "Can't load pointer.ive. Check osgDB data file search path." << std::endl;
    }
    else
    {
        m_pointer = handFile;
        m_pointer->setName( "Pointer" );

        if( !m_pointer.valid() )
        {
            std::cerr << "|\tProblems loading right hand model for glove tools." << std::endl;
        }
        else
        {
            m_pointerDCS = new ves::xplorer::scenegraph::DCS();
            m_pointerDCS->addChild( m_pointer.get() );
            pos.set( 0, 3, 3 );
            m_pointerDCS->setPosition( pos );
            m_pointerDCS->setAttitude( quat );
            osg::Vec3 scale( 0.00328, 0.00328, 0.00328 );
            m_pointerDCS->setScale( scale );

            ves::xplorer::scenegraph::util::NormalizeVisitor normVis( m_pointerDCS, true );
        }
    }
    /////////
    std::cout << "|\tInitialize Pointer" << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
Pointer::~Pointer()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Pointer::ProcessEvents( ves::open::xml::CommandPtr )
{
    if( !m_enabled )
    {
        return;
    }
    UpdatePointer();

    vprDEBUG( vesDBG, 3 ) << "|\tEnd Pointer::ProcessEvents" << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void Pointer::SetStartEndPoint( osg::Vec3d*, osg::Vec3d* )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Pointer::SetHeadRotationFlag( int input )
{
    rotationFlag = input;
}
////////////////////////////////////////////////////////////////////////////////
void Pointer::SelectObject()
{
    osg::Vec3d startPoint, endPoint;
    SetupStartEndPoint( &startPoint, &endPoint );

    beamLineSegment->set( startPoint, endPoint );

    osgUtil::IntersectVisitor objectBeamIntersectVisitor;
    objectBeamIntersectVisitor.addLineSegment( beamLineSegment.get() );

    //Add the IntersectVisitor to the root Node so that all all geometry will be
    //checked and no transforms are done to the Line segement.
    ves::xplorer::scenegraph::SceneManager::instance()->GetRootNode()->accept( objectBeamIntersectVisitor );

    osgUtil::IntersectVisitor::HitList beamHitList;
    beamHitList = objectBeamIntersectVisitor.getHitList( beamLineSegment.get() );

    ProcessHit( beamHitList );
}
////////////////////////////////////////////////////////////////////////////////
void Pointer::ProcessHit( osgUtil::IntersectVisitor::HitList listOfHits )
{
    osgUtil::Hit objectHit;
    selectedGeometry = 0;

    if( listOfHits.empty() )
    {
        vprDEBUG( vesDBG, 1 ) << "|\tPointer::ProcessHit No object selected"
                              << std::endl << vprDEBUG_FLUSH;

        ves::xplorer::DeviceHandler::instance()->SetActiveDCS(
            ves::xplorer::scenegraph::SceneManager::instance()->GetNavDCS() );

        return;
    }

    //Search for first item that is not the laser
    for( size_t i = 0; i <  listOfHits.size(); ++i )
    {
        objectHit = listOfHits[ i ];
        //std::cout << i << " " <<  objectHit._geode->getName() << std::endl;
        if( ( objectHit._geode->getName() != laserName ) &&
                ( objectHit._geode->getName() != "Root Node" ) )
        {
            break;
        }
    }

    //Make sure it is good
    if( !objectHit._geode.valid() )
    {
        return;
    }

    //Now find the id for the cad
    selectedGeometry = objectHit._geode;
    ves::xplorer::scenegraph::FindParentsVisitor parentVisitor( selectedGeometry.get() );
    osg::ref_ptr< osg::Node > parentNode = parentVisitor.GetParentNode();
    if( parentNode.valid() )
    {
        vprDEBUG( vesDBG, 1 ) << "|\tObjects has name "
                              << parentNode->getName() << std::endl << vprDEBUG_FLUSH;

        vprDEBUG( vesDBG, 1 ) << "|\tObjects descriptors "
                              << parentNode->getDescriptions().at( 1 )
                              << std::endl << vprDEBUG_FLUSH;

        ves::xplorer::DeviceHandler::instance()->SetActiveDCS(
            dynamic_cast< ves::xplorer::scenegraph::DCS* >( parentNode.get() ) );
    }
    else
    {
        selectedGeometry = objectHit._geode;

        vprDEBUG( vesDBG, 1 ) << "|\tObject does not have name parent name "
                              << objectHit._geode->getParents().front()->getName()
                              << std::endl << vprDEBUG_FLUSH;

        ves::xplorer::DeviceHandler::instance()->SetActiveDCS(
            ves::xplorer::scenegraph::SceneManager::instance()->GetNavDCS() );
    }
}
////////////////////////////////////////////////////////////////////////////////
//This function currently deletes the existing beam each time it is called and
//add a new beam.  This should be replaced such that it is only called once
//and then a transform is modified for the location.
void Pointer::DrawLine( osg::Vec3d start, osg::Vec3d end )
{
    if( beamGeode.valid() )
    {
        mRootNode->asGroup()->removeChild( beamGeode.get() );
    }

    beamGeode = new osg::Geode();
    beamGeometry = new osg::Geometry();
    beamGeode->addDrawable( beamGeometry.get() );
    beamGeode->setName( this->laserName );

    mRootNode->asGroup()->addChild( beamGeode.get() );

    osg::ref_ptr< osg::Vec3Array > beamVertices = new osg::Vec3Array;
    beamVertices->push_back(
        osg::Vec3( start[ 0 ] - 0.1, start[ 1 ], start[ 2 ] ) );
    beamVertices->push_back(
        osg::Vec3( start[ 0 ] + 0.1, start[ 1 ], start[ 2 ] ) );
    beamVertices->push_back(
        osg::Vec3( end[ 0 ] + 0.1, end[ 1 ], end[ 2 ] ) );
    beamVertices->push_back(
        osg::Vec3( end[ 0 ] - 0.1, end[ 1 ], end[ 2] ) );
    beamVertices->push_back(
        osg::Vec3( start[ 0 ] - 0.1, start[ 1 ], start[ 2 ] + 0.1 ) );
    beamVertices->push_back(
        osg::Vec3( start[ 0 ] + 0.1, start[ 1 ], start[ 2 ] + 0.1 ) );
    beamVertices->push_back(
        osg::Vec3( end[ 0 ] + 0.1, end[ 1 ], end[ 2 ] + 0.1 ) );
    beamVertices->push_back(
        osg::Vec3( end[ 0 ] - 0.1, end[ 1 ], end[ 2 ] + 0.1 ) );

    beamGeometry->setVertexArray( beamVertices.get() );

    osg::ref_ptr< osg::DrawElementsUInt > beamTop =
        new osg::DrawElementsUInt( osg::PrimitiveSet::QUADS, 0 );
    beamTop->push_back( 0 );
    beamTop->push_back( 1 );
    beamTop->push_back( 2 );
    beamTop->push_back( 3 );
    beamGeometry->addPrimitiveSet( beamTop.get() );

    osg::ref_ptr< osg::DrawElementsUInt > beamBottom =
        new osg::DrawElementsUInt( osg::PrimitiveSet::QUADS, 0 );
    beamBottom->push_back( 4 );
    beamBottom->push_back( 5 );
    beamBottom->push_back( 6 );
    beamBottom->push_back( 7 );
    beamGeometry->addPrimitiveSet( beamBottom.get() );

    osg::ref_ptr< osg::DrawElementsUInt > beamLeft =
        new osg::DrawElementsUInt( osg::PrimitiveSet::QUADS, 0 );
    beamLeft->push_back( 0 );
    beamLeft->push_back( 3 );
    beamLeft->push_back( 7 );
    beamLeft->push_back( 4 );
    beamGeometry->addPrimitiveSet( beamLeft.get() );

    osg::ref_ptr< osg::DrawElementsUInt > beamRight =
        new osg::DrawElementsUInt( osg::PrimitiveSet::QUADS, 0 );
    beamRight->push_back( 5 );
    beamRight->push_back( 6 );
    beamRight->push_back( 2 );
    beamRight->push_back( 1 );
    beamGeometry->addPrimitiveSet( beamRight.get() );

    osg::ref_ptr< osg::DrawElementsUInt > beamBack =
        new osg::DrawElementsUInt( osg::PrimitiveSet::QUADS, 0 );
    beamBack->push_back( 1 );
    beamBack->push_back( 0 );
    beamBack->push_back( 4 );
    beamBack->push_back( 5 );
    beamGeometry->addPrimitiveSet( beamBack.get() );

    osg::ref_ptr< osg::DrawElementsUInt > beamFront =
        new osg::DrawElementsUInt( osg::PrimitiveSet::QUADS, 0 );
    beamFront->push_back( 3 );
    beamFront->push_back( 2 );
    beamFront->push_back( 6 );
    beamFront->push_back( 7 );
    beamGeometry->addPrimitiveSet( beamFront.get() );

    osg::ref_ptr< osg::Vec4Array > colors = new osg::Vec4Array;
    colors->push_back( osg::Vec4( 1.0f, 0.0f, 1.0f, 1.0f ) );

    osg::ref_ptr< osg::UIntArray > cfdColorIndexArray = new osg::UIntArray();
    cfdColorIndexArray->push_back( 0 );

    beamGeometry->setColorArray( colors.get() );
    beamGeometry->setColorIndices( cfdColorIndexArray.get() );
    beamGeometry->setColorBinding( osg::Geometry::BIND_OVERALL );
}
////////////////////////////////////////////////////////////////////////////////
//The current implemention uses VJButton0 to select an object then if VJButton3
//is pressed the object is translated with the users motion.  Later the
//selection method will occur from voice commands and translation will be done
//using gestures from a glove.
void Pointer::UpdateObjectHandler()
{
    vprDEBUG( vesDBG, 3 ) << "|\tStart Pointer::UpdateObjectHandler"
                          << std::endl << vprDEBUG_FLUSH;

    //Update the juggler location of the wand
    UpdateWandLocalDirection();
    UpdateWandGlobalLocation();
    //UpdateDeltaWandPosition();

    //Now draw the new line location and setup the data for the hit list pointer
    osg::Vec3d startPoint, endPoint;
    SetupStartEndPoint( &startPoint, &endPoint );
    DrawLine( startPoint, endPoint );

    //Now select and object based on the new wand location
    if( digital[ 0 ]->getData() == gadget::Digital::TOGGLE_ON )
    {
        SelectObject();
        //Set delta back to 0 so that it is not moved
        //by the old delta from the previous frame
        //UpdateDeltaWandPosition();
    }

    /*
    //Now we can move the object if the button
    int buttonData = digital[ 0 ]->getData();
    if( ( buttonData == gadget::Digital::ON ) && selectedGeometry.valid() )
    {
        TranslateObject();
    }
    */

    vprDEBUG( vesDBG, 3 ) << "|\tEnd Pointer::UpdateObjectHandler"
                          << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void Pointer::SetupStartEndPoint( osg::Vec3d* startPoint, osg::Vec3d* endPoint )
{
    double* wandPosition = GetObjLocation();
    double* wandDirection = GetDirection();
    double wandEndPoint[ 3 ];

    for( int i = 0; i < 3; ++i )
    {
        wandEndPoint[ i ] = ( wandDirection [ i ] * distance );
    }

    startPoint->set( wandPosition[ 0 ], wandPosition[ 1 ], wandPosition[ 2 ] );
    endPoint->set( wandEndPoint[ 0 ], wandEndPoint[ 1 ], wandEndPoint[ 2 ] );
}
////////////////////////////////////////////////////////////////////////////////
void Pointer::TranslateObject()
{
    ves::xplorer::scenegraph::DCS* const activeDCS =
        ves::xplorer::DeviceHandler::instance()->GetActiveDCS();

    //double* wandPosition = GetObjLocation();
    osg::Vec3d offsetFromLastPosition;

    double* tempWorldRot = activeDCS->GetRotationArray();
    double worldRot[ 3 ];
    worldRot[ 0 ] = tempWorldRot[ 0 ];
    worldRot[ 1 ] = tempWorldRot[ 1 ];
    worldRot[ 2 ] = tempWorldRot[ 2 ];

    double* tempWorldTrans = activeDCS->GetVETranslationArray();
    double worldTrans[ 3 ];
    worldTrans[ 0 ] = tempWorldTrans[ 0 ];
    worldTrans[ 1 ] = tempWorldTrans[ 1 ];
    worldTrans[ 2 ] = tempWorldTrans[ 2 ];

    for( int i = 0; i < 3; ++i )
    {
        worldTrans[ i ] = deltaTrans[ i ] + worldTrans[ i ];
    }
}
////////////////////////////////////////////////////////////////////////////////
void Pointer::UpdateWandLocalDirection()
{
    //Get the normalized direction relative to the juggler frame
    gmtl::Vec3d vjVec;
    vjVec.set( 0.0f, 0.0f, -1.0f );
    Matrix44d vjMat = gmtl::convertTo< double >( wand->getData() );

    gmtl::xform( vjVec, vjMat, vjVec );
    gmtl::normalize( vjVec );

    //Transform from juggler to osg...
    dir[0] =  vjVec[ 0 ];
    dir[1] = -vjVec[ 2 ];
    dir[2] =  vjVec[ 1 ];
}
////////////////////////////////////////////////////////////////////////////////
void Pointer::UpdateWandGlobalLocation()
{
    //Transform wand point into global space get juggler Matrix of worldDCS
    //Note:: for osg we are in z up land
    gmtl::Point3d loc_temp, osgPointLoc;
    Matrix44d vjMat = gmtl::convertTo< double >( wand->getData() );

    gmtl::setTrans( loc_temp, vjMat );
    osgPointLoc[ 0 ] =  loc_temp[ 0 ];
    osgPointLoc[ 1 ] = -loc_temp[ 2 ];
    osgPointLoc[ 2 ] =  loc_temp[ 1 ];

    for( size_t i = 0; i < 3; ++i )
    {
        LastWandPosition[ i ] = objLoc[ i ];
        //cursorLoc[ i ] = this->loc[ i ];// + this->dir[ i ] * this->cursorLen;
        objLoc[ i ] = osgPointLoc[ i ];
    }
}
////////////////////////////////////////////////////////////////////////////////
double* Pointer::GetObjLocation()
{
    return objLoc;
}
////////////////////////////////////////////////////////////////////////////////
double* Pointer::GetDirection()
{
    return dir;
}
////////////////////////////////////////////////////////////////////////////////
void Pointer::UpdateDeltaWandPosition()
{
    for( size_t i = 0; i < 3; ++i )
    {
        deltaTrans[ i ] = objLoc[ i ] - LastWandPosition[ i ];
    }
}
////////////////////////////////////////////////////////////////////////////////
void Pointer::FreeRotateAboutWand( const bool freeRotate )
{
    ves::xplorer::scenegraph::DCS* const activeDCS =
        ves::xplorer::DeviceHandler::instance()->GetActiveDCS();

    gmtl::Matrix44d vrjWandMat = gmtl::convertTo< double >( wand->getData() );
    gmtl::Quatd wandQuat = gmtl::make< gmtl::Quatd >( vrjWandMat );

    osg::Vec3d tempVec( 0, 0, wandQuat[ 1 ] );
    m_rotIncrement =
        osg::Quat( osg::DegreesToRadians( -rotationStepSize ), tempVec );

    //If we are rotating about the users position
    if( !rotationFlag )
    {
        return;
    }

    vjHeadMat = gmtl::convertTo< double >( head->getData() );

    //Get juggler Matrix of worldDCS
    //Note:: for osg we are in z up land
    Matrix44d worldMat;
    worldMat = activeDCS->GetMat();

    gmtl::Point3d jugglerHeadPoint, jugglerHeadPointTemp;
    jugglerHeadPoint = gmtl::makeTrans< gmtl::Point3d >( vjHeadMat );
    jugglerHeadPointTemp[ 0 ] = jugglerHeadPoint[ 0 ];
    jugglerHeadPointTemp[ 1 ] = -jugglerHeadPoint[ 2 ];
    jugglerHeadPointTemp[ 2 ] = 0;//jugglerHeadPoint[ 1 ];

    //Translate world dcs by distance that the head
    // is away from the origin
    gmtl::Matrix44d transMat =
        gmtl::makeTrans< gmtl::Matrix44d >( -jugglerHeadPointTemp );
    gmtl::Matrix44d worldMatTrans = transMat * worldMat;
    gmtl::Point3d newJugglerHeadPoint;

    //Get the position of the head in the new world space
    //as if the head is on the origin
    gmtl::Point3d newGlobalHeadPointTemp =
        worldMatTrans * newJugglerHeadPoint;

    //Create rotation matrix and juggler head vector
    if( freeRotate )
    {
        //Rotate about arbitrary axis
        tempVec.set( wandQuat[ 0 ], -wandQuat[ 2 ], wandQuat[ 1 ] );
    }
    else
    {
        //Rotate about z-up axis
        tempVec.set( 0, 0, wandQuat[ 1 ] );
    }

    //Create rotation increment
    m_rotIncrement =
        osg::Quat( osg::DegreesToRadians( -rotationStepSize ), tempVec );
    //Now make it a 4 x 4
    gmtl::Matrix44d rotMatTemp = gmtl::make< gmtl::Matrix44d >
                                 ( gmtl::Quat< double >( m_rotIncrement[ 0 ],
                                         m_rotIncrement[ 1 ], m_rotIncrement[ 2 ],
                                         m_rotIncrement[ 3 ] ) );

    gmtl::Vec4d newGlobalHeadPointVec;
    newGlobalHeadPointVec[ 0 ] = newGlobalHeadPointTemp[ 0 ];
    newGlobalHeadPointVec[ 1 ] = newGlobalHeadPointTemp[ 1 ];
    newGlobalHeadPointVec[ 2 ] = newGlobalHeadPointTemp[ 2 ];

    //Rotate the head vector by the rotation increment
    gmtl::Vec4d rotateJugglerHeadVec =
        rotMatTemp * newGlobalHeadPointVec;

    //Create translation from new rotated point
    //and add original head off set to the newly found location
    //Set world translation accordingly
    m_worldTrans[ 0 ] =
        -( rotateJugglerHeadVec[ 0 ] + jugglerHeadPointTemp[ 0 ] );
    m_worldTrans[ 1 ] =
        -( rotateJugglerHeadVec[ 1 ] + jugglerHeadPointTemp[ 1 ] );
    m_worldTrans[ 2 ] =
        -( rotateJugglerHeadVec[ 2 ] + jugglerHeadPointTemp[ 2 ] );
}
////////////////////////////////////////////////////////////////////////////////
double* Pointer::GetPlaneEquationConstantsNormalToWand()
{
    ///Get wand pointing vector
    Matrix44d vjMat = gmtl::convertTo< double >( wand->getData() );
    ///Transform from juggler space to world space
    Matrix44d worldWandMat =
        ves::xplorer::scenegraph::SceneManager::instance()->GetNavDCS()->GetMat() * vjMat;
    ///Normalize vector
    gmtl::Vec3d vjVec;
    vjVec.set( 0.0f, 0.0f, -1.0f );
    gmtl::xform( vjVec, worldWandMat, vjVec );
    gmtl::normalize( vjVec );
    ///Transform from juggler to osg...
    m_planeConstants[0] =  vjVec[ 0 ];
    m_planeConstants[1] = -vjVec[ 2 ];
    m_planeConstants[2] =  vjVec[ 1 ];
    m_planeConstants[3] =  4;
    return m_planeConstants;
}
////////////////////////////////////////////////////////////////////////////////
void Pointer::UpdateHandModel()
{
    /*
     bool handle( const osgGA::GUIEventAdapter& ea, osgGA::GUIActionAdapter& )
     {
     const unsigned int mod = ea.getModKeyMask();
     const bool ctrl = ( (mod&osgGA::GUIEventAdapter::MODKEY_LEFT_CTRL) ||
     (mod&osgGA::GUIEventAdapter::MODKEY_RIGHT_CTRL) );
     const bool alt = ( (mod&osgGA::GUIEventAdapter::MODKEY_LEFT_SHIFT) ||
     (mod&osgGA::GUIEventAdapter::MODKEY_RIGHT_SHIFT) );

     const unsigned int buttonMask( ea.getButtonMask() );
     const bool ourLeft( (ctrl || alt) && (buttonMask == osgGA::GUIEventAdapter::LEFT_MOUSE_BUTTON) );

     switch( ea.getEventType() )
     {
     case osgGA::GUIEventAdapter::KEYUP:
     {
     if (ea.getKey()==osgGA::GUIEventAdapter::KEY_Home)
     {
     _hand->setPose( osgbBullet::HandNode::POSE_DEFAULT );
     return true;
     }
     else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_End)
     {
     _hand->setPose( osgbBullet::HandNode::POSE_HOOK );
     return true;
     }
     else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_Page_Up)
     {
     _hand->setPose( osgbBullet::HandNode::POSE_POINT );
     return true;
     }
     else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_Page_Down)
     {
     _hand->setPose( osgbBullet::HandNode::POSE_FIST );
     return true;
     }
     else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_Delete)
     {
     _hand->dump();
     return true;
     }
     else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F1)
     {
     _mode = osgbBullet::HandNode::FINGER_0_TRANSLATE;
     return true;
     }
     else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F2)
     {
     _mode = osgbBullet::HandNode::FINGER_1_TRANSLATE;
     return true;
     }
     else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F3)
     {
     _mode = osgbBullet::HandNode::FINGER_2_TRANSLATE;
     return true;
     }
     else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F4)
     {
     _mode = osgbBullet::HandNode::FINGER_3_TRANSLATE;
     return true;
     }
     else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F5)
     {
     _mode = osgbBullet::HandNode::FINGER_4_TRANSLATE;
     return true;
     }
     else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F6)
     {
     _mode = osgbBullet::HandNode::MAX_ARTICULATIONS;
     return true;
     }

     else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_Left)
     {
     if( _mode == osgbBullet::HandNode::MAX_ARTICULATIONS )
     {
     osgbBullet::HandNode::Articulation art;
     for( art=osgbBullet::HandNode::FINGER_0_TRANSLATE;
     art<=osgbBullet::HandNode::FINGER_4_TRANSLATE; art++ )
     {
     _hand->setArticulation( art,
     _hand->getArticulation( art ) + 0.1 );
     }
     }
     else
     _hand->setArticulation( _mode,
     _hand->getArticulation( _mode ) + 0.1 );
     return true;
     }
     else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_Right)
     {
     if( _mode == osgbBullet::HandNode::MAX_ARTICULATIONS )
     {
     osgbBullet::HandNode::Articulation art;
     for( art=osgbBullet::HandNode::FINGER_0_TRANSLATE;
     art<=osgbBullet::HandNode::FINGER_4_TRANSLATE; art++ )
     {
     _hand->setArticulation( art,
     _hand->getArticulation( art ) - 0.1 );
     }
     }
     else
     _hand->setArticulation( _mode,
     _hand->getArticulation( _mode ) - 0.1 );
     return true;
     }
     return false;
     }

     case osgGA::GUIEventAdapter::SCROLL:
     {
     const unsigned int mod = ea.getModKeyMask();
     const bool k1 = ( (mod&osgGA::GUIEventAdapter::MODKEY_LEFT_CTRL) ||
     (mod&osgGA::GUIEventAdapter::MODKEY_RIGHT_CTRL) );
     const bool k0 = ( !k1 || ( (mod&osgGA::GUIEventAdapter::MODKEY_LEFT_SHIFT) ||
     (mod&osgGA::GUIEventAdapter::MODKEY_RIGHT_SHIFT) ) );

     float delta( 0.05 );
     osgGA::GUIEventAdapter::ScrollingMotion sm = ea.getScrollingMotion();
     if (sm == osgGA::GUIEventAdapter::SCROLL_UP)
     delta = -delta;

     if( _mode == osgbBullet::HandNode::MAX_ARTICULATIONS )
     {
     osgbBullet::HandNode::Articulation art;
     for( art=osgbBullet::HandNode::FINGER_0_TRANSLATE;
     art<=osgbBullet::HandNode::FINGER_4_TRANSLATE; art++ )
     {
     if (k0) _hand->setArticulation( art + 5 , _hand->getArticulation( art+5  ) + delta );
     if (k1) _hand->setArticulation( art + 10, _hand->getArticulation( art+10 ) + delta );
     }
     }
     else
     {
     if (k0) _hand->setArticulation( _mode + 5 , _hand->getArticulation( _mode+5  ) + delta );
     if (k1) _hand->setArticulation( _mode + 10, _hand->getArticulation( _mode+10 ) + delta );
     }
     return true;
     }
     case osgGA::GUIEventAdapter::PUSH:
     {
     if( !ourLeft )
     return false;

     _lastX = ea.getXnormalized();
     _lastY = ea.getYnormalized();
     return true;
     }
     case osgGA::GUIEventAdapter::DRAG:
     {
     if( !ourLeft )
     return false;

     osg::Vec3 move;
     if( ctrl )
     {
     move[ 0 ] = _lastX - ea.getXnormalized();
     move[ 1 ] = _lastY - ea.getYnormalized();
     }
     else if( alt )
     move[ 2 ] = ea.getYnormalized() - _lastY;
     _lastX = ea.getXnormalized();
     _lastY = ea.getYnormalized();

     osg::Quat q = _hand->getAttitude();
     osg::Vec3 tmove = q * move * 5.f;
     _hand->setPosition( tmove + _hand->getPosition() );
     return true;
     }
     default:
     break;
     }
     return false;
     }
    */
}
////////////////////////////////////////////////////////////////////////////////
void Pointer::UpdatePointer()
{
    if( !m_pointerDCS.valid() )
    {
        return;
    }

    if( !mRootNode->containsNode( m_pointerDCS.get() ) )
    {
        return;
    }

    gmtl::Matrix44d hand_pos_rot;
    hand_pos_rot[0][3] = 0;
    hand_pos_rot[1][3] = 3;
    hand_pos_rot[2][3] = -3;
    if( !m_pointerPosition->isStupefied() )
    {
        //Get data from the trackers
        gmtl::Matrix44f tempHand = m_pointerPosition->getData();
        hand_pos_rot = gmtl::convertTo< double >( tempHand );
    }

    //gmtl::Vec3d x_axis( 1.0f, 0.0f, 0.0f );
    //gmtl::Matrix44d rhRot = gmtl::makeRot< gmtl::Matrix44d >(
    //    gmtl::AxisAngled( gmtl::Math::deg2Rad( 90.0f ), x_axis ) );
    gmtl::Matrix44d tempCamera =
        ves::xplorer::scenegraph::SceneManager::instance()->GetInvertedNavMatrix();
    hand_pos_rot = tempCamera * hand_pos_rot;
    m_pointerDCS->setPosition( osg::Vec3( hand_pos_rot[0][3], hand_pos_rot[1][3], hand_pos_rot[2][3] ) );

    //gmtl::Matrix44d vrjRHandMat = gmtl::convertTo< double >( hand_pos_rot );
    //osg::Vec3d pitch( 1, 0, 0 );
    //osg::Vec3d roll( 0, 1, 0 );
    //osg::Vec3d yaw( 0, 0, 1 );

    //osg::Matrixd rotateMat;
    //rotateMat.makeRotate( osg::DegreesToRadians( 180.0 ), yaw,
    //                     osg::DegreesToRadians( -90.0 ), pitch,
    //                     osg::DegreesToRadians( 0.0 ), roll );

    double rotArray[ 3 ] = { 0.0, -90.0, 0.0 };
    osg::Matrixd rotateMat = CreateQuat( rotArray );

    gmtl::Matrix44d naVRot;
    naVRot.set( rotateMat.ptr() );
    hand_pos_rot = hand_pos_rot * naVRot;
    gmtl::Quatd rhandQuat = gmtl::make< gmtl::Quatd >( hand_pos_rot );

    m_pointerDCS->setAttitude( osg::Quat( rhandQuat[0], rhandQuat[1], rhandQuat[2], rhandQuat[3] ) );


    /*
     DCS& dcs = cameraObject->GetDCS();
     const gmtl::AxisAngled myAxisAngle(
     osg::DegreesToRadians( double( -90 ) ), 1, 0, 0 );
     gmtl::Matrix44d myMat = gmtl::make< gmtl::Matrix44d >( myAxisAngle );
     ///We need to rotate the camera geometry 90 initially so that the geometry
     ///is in VR Juggler space (y up) so that when the view matrix is multiplied
     ///in the 90 is taken back out.
     myMat = ves::xplorer::scenegraph::SceneManager::instance()->
     GetGlobalViewMatrix() * myMat;
     dcs.SetMat( myMat );
    */
}
////////////////////////////////////////////////////////////////////////////////
osg::Matrix Pointer::CreateQuat( double* rotArray )
{
    // We now have h, p, and r angles. Build a Quat to affect these rotatiions.
    // We do this by creating a Matrix that contains correctly-oriented x, y, and
    // z axes. Then we create the Quat from the Matrix.
    //
    // First, create x, y, and z axes that represent the h, p, and r angles.
    //   Rotate x and y axes by the heading.
    osg::Vec3 z( 0., 0., 1. );
    osg::Quat qHeading( osg::DegreesToRadians( rotArray[0] ), z );
    osg::Vec3 x = qHeading * osg::Vec3( 1., 0., 0. );
    osg::Vec3 y = qHeading * osg::Vec3( 0., 1., 0. );
    //   Rotate z and y axes by the pitch.
    osg::Quat qPitch( osg::DegreesToRadians( rotArray[1] ), x );
    y = qPitch * y;
    z = qPitch * z;
    //   Rotate x and z axes by the roll.
    osg::Quat qRoll( osg::DegreesToRadians( rotArray[2] ), y );
    x = qRoll * x;
    z = qRoll * z;
    // Use x, y, and z axes to create an orientation matrix.
    osg::Matrix m( x[0], x[1], x[2], 0.,
                   y[0], y[1], y[2], 0.,
                   z[0], z[1], z[2], 0.,
                   0., 0., 0., 1. );

    //osg::Quat quat;
    //quat.set( m );
    //setAttitude( quat );
    //setPivotPoint( osg::Vec3d( 0, 0, 0 ) );

    return m;
}
