/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
#include <ves/xplorer/device/Gloves.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <ves/xplorer/Debug.h>
#include <ves/xplorer/DeviceHandler.h>

#include <ves/xplorer/environment/cfdEnum.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/FindParentsVisitor.h>

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
#include <osgDB/FileUtils>
#include <osgDB/FileNameUtils>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace gmtl;
using namespace gadget;
using namespace ves::xplorer::device;
using namespace ves::xplorer::scenegraph;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////////////////
Gloves::Gloves()
    :
    Device( GLOVES ),
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
    //Setup right hand
    {
        mRightHandPos.init( "GloveB-RightHand" );

        mRightThumbMCP.init("GloveB-ThumbMCP");
        mRightThumbPIP.init("GloveB-ThumbPIP");

        mRightThumbIndexAbduction.init("GloveB-ThumbIndexAbduction");

        mRightIndexMCP.init("GloveB-IndexMCP");
        mRightIndexPIP.init("GloveB-IndexPIP");

        mRightIndexMiddleAbduction.init("GloveB-IndexMiddleAbduction");

        mRightMiddleMCP.init("GloveB-MiddleMCP");
        mRightMiddlePIP.init("GloveB-MiddlePIP");
        mRightMiddleRingAbduction.init("GloveB-MiddleRingAbduction");

        mRightRingMCP.init("GloveB-RingMCP");
        mRightRingPIP.init("GloveB-RingPIP");

        mRightRingPinkyAbduction.init("GloveB-RingPinkyAbduction");

        mRightPinkyMCP.init("GloveB-PinkyMCP");
        mRightPinkyPIP.init("GloveB-PinkyPIP");
    }
    //Setup left hand
    {
        mLeftHandPos.init( "GloveA-LeftHand" );

        mLeftThumbMCP.init("GloveA-ThumbMCP");
        mLeftThumbPIP.init("GloveA-ThumbPIP");

        mLeftThumbIndexAbduction.init("GloveA-ThumbIndexAbduction");

        mLeftIndexMCP.init("GloveA-IndexMCP");
        mLeftIndexPIP.init("GloveA-IndexPIP");

        mLeftIndexMiddleAbduction.init("GloveA-IndexMiddleAbduction");

        mLeftMiddleMCP.init("GloveA-MiddleMCP");
        mLeftMiddlePIP.init("GloveA-MiddlePIP");
        mLeftMiddleRingAbduction.init("GloveA-MiddleRingAbduction");

        mLeftRingMCP.init("GloveA-RingMCP");
        mLeftRingPIP.init("GloveA-RingPIP");

        mLeftRingPinkyAbduction.init("GloveA-RingPinkyAbduction");

        mLeftPinkyMCP.init("GloveA-PinkyMCP");
        mLeftPinkyPIP.init("GloveA-PinkyPIP");
    }
    beamLineSegment = new osg::LineSegment();
    
    mRootNode = new osg::Group();
    mRootNode->setName( "Glove Group" );
    
    ves::xplorer::DeviceHandler::instance()->GetDeviceGroup()->addChild( mRootNode.get() );

    {
        std::string shaderName = osgDB::findDataFile( "null_glow_texture.fs" );
        osg::ref_ptr< osg::Shader > fragShader = 
            osg::Shader::readShaderFile( osg::Shader::FRAGMENT, shaderName );
        
        osg::ref_ptr< osg::Program > program = new osg::Program();
        program->addShader( fragShader.get() );
        
        osg::ref_ptr< osg::StateSet > stateset = 
            mRootNode->getOrCreateStateSet();
        stateset->setAttributeAndModes( program.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        
        stateset->addUniform( new osg::Uniform( "tex", 0 ) );
    }
    
    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
Gloves* Gloves::AsGloves()
{
    return this;
}
////////////////////////////////////////////////////////////////////////////////
void Gloves::Enable( const bool& enable )
{
    m_enabled = enable;

    if( m_enabled )
    {
        mRootNode->addChild( mRightHand.get() );
        mRootNode->addChild( mLeftHand.get() );
    }
    else
    {
        if( mRootNode->containsNode( mRightHand.get() ) )
        {
            mRootNode->removeChild( mRightHand.get() );
        }

        if( mRootNode->containsNode( mLeftHand.get() ) )
        {
            mRootNode->removeChild( mLeftHand.get() );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void Gloves::Initialize()
{
    for( int i = 0; i < 3; ++i )
    {
        cursorLoc[ i ] = 0;
        //loc[ i ] + dir[ i ] * cursorLen;
        objLoc[ i ] = cursorLoc[ i ];
    }
    
    osg::Vec3 pos;
    osg::Quat quat;
    
    float length = 0.8;

    /////////
    osg::ref_ptr< osg::Node > handFile = osgDB::readNodeFile( "handR.ive" );
    
    if( !handFile.valid() )
    {
        osg::notify( osg::FATAL ) << "HandNode: Can't load handR.ive. Check osgDB data file search path." << std::endl;
        //return;
    }
    else
    {
        mRightHand = new osgbInteraction::HandNode( ves::xplorer::scenegraph::PhysicsSimulator::instance()->GetDynamicsWorld(), osgbInteraction::HandNode::RIGHT, length );
        mRightHand->setName( "Right Hand Glove" );
        
        if( !mRightHand.valid() )
        {
            std::cerr << "|\tProblems loading right hand model for glove tools." << std::endl;
            //return;
        }
        else
        {
            pos.set( 0, 3, 3 );
            mRightHand->setPosition( pos );
            mRightHand->setAttitude( quat );
            mRightHand->setDebug( mDebugInfo );
        }
    }
    /////////
    
    /////////
    handFile = osgDB::readNodeFile( "handL.ive" );
    
    if( !handFile.valid() )
    {
        osg::notify( osg::FATAL ) << "HandNode: Can't load handL.ive. Check osgDB data file search path." << std::endl;
        //return;
    }
    else
    {
        mLeftHand = new osgbInteraction::HandNode( ves::xplorer::scenegraph::PhysicsSimulator::instance()->GetDynamicsWorld(), osgbInteraction::HandNode::LEFT, length );
        mLeftHand->setName( "Left Hand Glove" );
        
        if( !mLeftHand.valid() )
        {
            std::cerr << "|\tProblems loading left hand model for glove tools." << std::endl;
            return;
        }
        else
        {
            pos.set( 3, 3, 3 );
            mLeftHand->setPosition( pos );
            mLeftHand->setAttitude( quat );
            mLeftHand->setDebug( mDebugInfo );
        }
    }
    /////////

    std::cout << "|\tInitialize Gloves" << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
Gloves::~Gloves()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Gloves::ProcessEvents( ves::open::xml::CommandPtr )
{
    if( !m_enabled )
    {
        return;
    }
    UpdateRightHandGlove();
    UpdateLeftHandGlove();

    vprDEBUG( vesDBG, 3 ) << "|\tEnd Gloves::ProcessEvents" << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void Gloves::SetStartEndPoint( osg::Vec3d*, osg::Vec3d* )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Gloves::SetHeadRotationFlag( int input )
{
    rotationFlag = input;
}
////////////////////////////////////////////////////////////////////////////////
void Gloves::SelectObject()
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
void Gloves::ProcessHit( osgUtil::IntersectVisitor::HitList listOfHits )
{
    osgUtil::Hit objectHit;
    selectedGeometry = 0;
    
    if( listOfHits.empty() )
    {
        vprDEBUG( vesDBG, 1 ) << "|\tGloves::ProcessHit No object selected"
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
        if (( objectHit._geode->getName() != laserName ) &&
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
void Gloves::DrawLine( osg::Vec3d start, osg::Vec3d end )
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
void Gloves::UpdateObjectHandler()
{
    vprDEBUG( vesDBG, 3 ) << "|\tStart Gloves::UpdateObjectHandler"
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

    vprDEBUG( vesDBG, 3 ) << "|\tEnd Gloves::UpdateObjectHandler"
    << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void Gloves::SetupStartEndPoint( osg::Vec3d * startPoint, osg::Vec3d * endPoint )
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
void Gloves::TranslateObject()
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
void Gloves::UpdateWandLocalDirection()
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
void Gloves::UpdateWandGlobalLocation()
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
double* Gloves::GetObjLocation()
{
    return objLoc;
}
////////////////////////////////////////////////////////////////////////////////
double* Gloves::GetDirection()
{
    return dir;
}
////////////////////////////////////////////////////////////////////////////////
void Gloves::UpdateDeltaWandPosition()
{
    for( size_t i = 0; i < 3; ++i )
    {
        deltaTrans[ i ] = objLoc[ i ] - LastWandPosition[ i ];
    }
}
////////////////////////////////////////////////////////////////////////////////
void Gloves::FreeRotateAboutWand( const bool freeRotate )
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
double* Gloves::GetPlaneEquationConstantsNormalToWand()
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
void Gloves::UpdateHandModel()
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
void Gloves::UpdateRightHandGlove()
{
    if( !mRightHand.valid() )
    {
        return;
    }
    
    if( !mRootNode->containsNode( mRightHand.get() ) )
    {
        return;
    }
        
    //Get data from hand joints
        //Get all the VR Juggler data variables
    //Update the hand joints

    float pi_over_2 = 3.14 / 180.0;
    float mcp_scale = 80.f * pi_over_2;
    float pip_scale = 110.f * pi_over_2;
    mRightHand->setArticulation( osgbInteraction::HandNode::FINGER_0_ROTATE_INNER, mRightThumbMCP->getData() * mcp_scale );
    mRightHand->setArticulation( osgbInteraction::HandNode::FINGER_1_ROTATE_INNER, mRightIndexMCP->getData() * mcp_scale );
    mRightHand->setArticulation( osgbInteraction::HandNode::FINGER_2_ROTATE_INNER, mRightMiddleMCP->getData() * mcp_scale );
    mRightHand->setArticulation( osgbInteraction::HandNode::FINGER_3_ROTATE_INNER, mRightRingMCP->getData() * mcp_scale );
    mRightHand->setArticulation( osgbInteraction::HandNode::FINGER_4_ROTATE_INNER, mRightPinkyMCP->getData() * mcp_scale );

    mRightHand->setArticulation( osgbInteraction::HandNode::FINGER_0_ROTATE_OUTER, mRightThumbPIP->getData() * pip_scale );
    mRightHand->setArticulation( osgbInteraction::HandNode::FINGER_1_ROTATE_OUTER, mRightIndexPIP->getData() * pip_scale );
    mRightHand->setArticulation( osgbInteraction::HandNode::FINGER_2_ROTATE_OUTER, mRightMiddlePIP->getData() * pip_scale );
    mRightHand->setArticulation( osgbInteraction::HandNode::FINGER_3_ROTATE_OUTER, mRightRingPIP->getData() * pip_scale );
    mRightHand->setArticulation( osgbInteraction::HandNode::FINGER_4_ROTATE_OUTER, mRightPinkyPIP->getData() * pip_scale );

    //float thumb_scale = 60.f * pi_over_2;
    //float abduction_scale = 30.f * pi_over_2;
    //float angle1 = mRightThumbIndexAbduction->getData() * thumb_scale;
    //float angle2 = mRightIndexMiddleAbduction->getData() * abduction_scale;
    //float angle3 = 0.0f;//mRightMiddleRingAbduction->getData() * abduction_scale + angle2 ;
    //float angle4 = mRightMiddleRingAbduction->getData() * abduction_scale;
    //float angle5 = mRightRingPinkyAbduction->getData() * abduction_scale;

    //mRightHand->setArticulation( osgbBullet::HandNode::FINGER_0_TRANSLATE, angle1 );
    //mRightHand->setArticulation( osgbBullet::HandNode::FINGER_1_TRANSLATE, angle2 );
    //mRightHand->setArticulation( osgbBullet::HandNode::FINGER_2_TRANSLATE, angle3 );
    //mRightHand->setArticulation( osgbBullet::HandNode::FINGER_3_TRANSLATE, -angle4 );
    //mRightHand->setArticulation( osgbBullet::HandNode::FINGER_4_TRANSLATE, -angle5 );

    //mRightHand->setArticulation( _mode, mRightHand->getArticulation( _mode ) + 0.1 );
    //mLeftHand->setArticulation( _mode, mLeftHand->getArticulation( _mode ) + 0.1 );

    gmtl::Matrix44d hand_pos_rot;
    hand_pos_rot[0][3] = 0;
    hand_pos_rot[1][3] = 3;
    hand_pos_rot[2][3] = -3;
    if( !mRightHandPos->isStupefied() )
    {
        //Get data from the trackers
        gmtl::Matrix44f tempHand = mRightHandPos->getData();
        hand_pos_rot = gmtl::convertTo< double >( tempHand );
    }
    else
    {
        //return;
    }

    //gmtl::Vec3d x_axis( 1.0f, 0.0f, 0.0f );
    //gmtl::Matrix44d rhRot = gmtl::makeRot< gmtl::Matrix44d >(
    //    gmtl::AxisAngled( gmtl::Math::deg2Rad( 90.0f ), x_axis ) );
    gmtl::Matrix44d tempCamera = 
        ves::xplorer::scenegraph::SceneManager::instance()->GetInvertedNavMatrix();
    hand_pos_rot = tempCamera * hand_pos_rot;
    mRightHand->setPosition( osg::Vec3( hand_pos_rot[0][3], hand_pos_rot[1][3], hand_pos_rot[2][3] ) );
    
    double rotArray[ 3 ] = { 180.0, 90.0, 0.0 };
    osg::Matrixd rotateMat = CreateQuat( rotArray );
    
    gmtl::Matrix44d naVRot;
    naVRot.set( rotateMat.ptr() );
    hand_pos_rot = hand_pos_rot * naVRot;
    gmtl::Quatd rhandQuat = gmtl::make< gmtl::Quatd >( hand_pos_rot );

    mRightHand->setAttitude( osg::Quat(rhandQuat[0], rhandQuat[1], rhandQuat[2], rhandQuat[3]  ) );
}
////////////////////////////////////////////////////////////////////////////////
void Gloves::UpdateLeftHandGlove()
{
    if( !mLeftHand.valid() )
    {
        return;
    }
    
    if( !mRootNode->containsNode( mLeftHand.get() ) )
    {
        return;
    }

    //Get data from hand joints
        //Get all the VR Juggler data variables
    //Update the hand joints

    float pi_over_2 = 3.14 / 180.0;
    float mcp_scale = 80.f * pi_over_2;
    float pip_scale = 110.f * pi_over_2;
    mLeftHand->setArticulation( osgbInteraction::HandNode::FINGER_0_ROTATE_INNER, mLeftThumbMCP->getData() * mcp_scale );
    mLeftHand->setArticulation( osgbInteraction::HandNode::FINGER_1_ROTATE_INNER, mLeftIndexMCP->getData() * mcp_scale );
    mLeftHand->setArticulation( osgbInteraction::HandNode::FINGER_2_ROTATE_INNER, mLeftMiddleMCP->getData() * mcp_scale );
    mLeftHand->setArticulation( osgbInteraction::HandNode::FINGER_3_ROTATE_INNER, mLeftRingMCP->getData() * mcp_scale );
    mLeftHand->setArticulation( osgbInteraction::HandNode::FINGER_4_ROTATE_INNER, mLeftPinkyMCP->getData() * mcp_scale );

    mLeftHand->setArticulation( osgbInteraction::HandNode::FINGER_0_ROTATE_OUTER, mLeftThumbPIP->getData() * pip_scale );
    mLeftHand->setArticulation( osgbInteraction::HandNode::FINGER_1_ROTATE_OUTER, mLeftIndexPIP->getData() * pip_scale );
    mLeftHand->setArticulation( osgbInteraction::HandNode::FINGER_2_ROTATE_OUTER, mLeftMiddlePIP->getData() * pip_scale );
    mLeftHand->setArticulation( osgbInteraction::HandNode::FINGER_3_ROTATE_OUTER, mLeftRingPIP->getData() * pip_scale );
    mLeftHand->setArticulation( osgbInteraction::HandNode::FINGER_4_ROTATE_OUTER, mLeftPinkyPIP->getData() * pip_scale );

    //float thumb_scale = 60.f * pi_over_2;
    //float abduction_scale = 30.f * pi_over_2;
    //float angle1 = mRightThumbIndexAbduction->getData() * thumb_scale;
    //float angle2 = mRightIndexMiddleAbduction->getData() * abduction_scale;
    //float angle3 = 0.0f;//mRightMiddleRingAbduction->getData() * abduction_scale + angle2 ;
    //float angle4 = mRightMiddleRingAbduction->getData() * abduction_scale;
    //float angle5 = mRightRingPinkyAbduction->getData() * abduction_scale;

    //mRightHand->setArticulation( osgbBullet::HandNode::FINGER_0_TRANSLATE, angle1 );
    //mRightHand->setArticulation( osgbBullet::HandNode::FINGER_1_TRANSLATE, angle2 );
    //mRightHand->setArticulation( osgbBullet::HandNode::FINGER_2_TRANSLATE, angle3 );
    //mRightHand->setArticulation( osgbBullet::HandNode::FINGER_3_TRANSLATE, -angle4 );
    //mRightHand->setArticulation( osgbBullet::HandNode::FINGER_4_TRANSLATE, -angle5 );

    //mRightHand->setArticulation( _mode, mRightHand->getArticulation( _mode ) + 0.1 );
    //mLeftHand->setArticulation( _mode, mLeftHand->getArticulation( _mode ) + 0.1 );

    gmtl::Matrix44d hand_pos_rot;
    hand_pos_rot[0][3] = 0;
    hand_pos_rot[1][3] = 3;
    hand_pos_rot[2][3] = -3;
    if( !mLeftHandPos->isStupefied() )
    {
        //Get data from the trackers
        gmtl::Matrix44f tempHand = mLeftHandPos->getData();
        hand_pos_rot = gmtl::convertTo< double >( tempHand );
    }
    else
    {
        //return;
    }
    
    //gmtl::Vec3d x_axis( 1.0f, 0.0f, 0.0f );
    //gmtl::Matrix44d rhRot = gmtl::makeRot< gmtl::Matrix44d >(
    //    gmtl::AxisAngled( gmtl::Math::deg2Rad( 90.0f ), x_axis ) );
    gmtl::Matrix44d tempCamera = 
    ves::xplorer::scenegraph::SceneManager::instance()->GetInvertedNavMatrix();
    hand_pos_rot = tempCamera * hand_pos_rot;
    mLeftHand->setPosition( osg::Vec3( hand_pos_rot[0][3], hand_pos_rot[1][3], hand_pos_rot[2][3] ) );
    
    double rotArray[ 3 ] = { 180.0, 90.0, 0.0 };
    osg::Matrixd rotateMat = CreateQuat( rotArray );

    gmtl::Matrix44d naVRot;
    naVRot.set( rotateMat.ptr() );
    hand_pos_rot = hand_pos_rot * naVRot;
    gmtl::Quatd rhandQuat = gmtl::make< gmtl::Quatd >( hand_pos_rot );
    
    mLeftHand->setAttitude( osg::Quat(rhandQuat[0], rhandQuat[1], rhandQuat[2], rhandQuat[3]  ) );
}
////////////////////////////////////////////////////////////////////////////////
osg::Matrix Gloves::CreateQuat( double* rotArray )
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
