/*************** <auto-copyright.pl BEGIN do not edit this line> **************
*
* VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
// --- VE-Suite Includes --- //
#include "VE_Xplorer/XplorerHandlers/Wand.h"

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"
#include "VE_Xplorer/XplorerHandlers/cfdEnum.h"

#include "VE_Xplorer/SceneGraph/SceneManager.h"
#include "VE_Xplorer/SceneGraph/FindParentsVisitor.h"

#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

// --- OSG Includes --- //
#include <osg/LineSegment>
#include <osg/Group>
#include <osg/Geometry>
#include <osg/MatrixTransform>
#include <osg/Array>
#include <osg/NodeVisitor>

// --- VR Juggler Stuff --- //
#include <gmtl/Xforms.h>
#include <gmtl/Generate.h>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace gmtl;
using namespace gadget;
using namespace VE_Xplorer;
using namespace VE_SceneGraph;

////////////////////////////////////////////////////////////////////////////////
Wand::Wand()
:
subzeroFlag( 0 ),
rotationFlag( 1 ),
distance( 1000 ),
cursorLen( 1.0f ),
command( 0 ),
translationStepSize( 0.25f ),
rotationStepSize( 1.0f ),
m_buttonPushed( false )
{
    wand.init( "VJWand" );
    head.init( "VJHead" );
    // trigger (and top right button) TODO: I think this is unused ?
    digital[ 0 ].init( "VJButton0" );
    // top left button -- toggle cursor mode: laser, streamlines, box, & arrow
    digital[ 1 ].init( "VJButton1" );
    // 12 o'clock -- forward navigation       
    digital[ 2 ].init( "VJButton2" );  
    // 3 o'clock -- not used at present     
    digital[ 3 ].init( "VJButton3" );
    // 6 o'clock -- reset      
    digital[ 4 ].init( "VJButton4" );
    // 9 o'clock -- exit streamer while loop     
    digital[ 5 ].init( "VJButton5" );       

    beamLineSegment = new osg::LineSegment();
    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
void Wand::Initialize()
{
    rootNode = VE_SceneGraph::SceneManager::instance()->GetRootNode();

    for( int i = 0; i < 3; ++i )
    {
        cursorLoc[ i ] = 0;
        //loc[ i ] + dir[ i ] * cursorLen;
        objLoc[ i ] = cursorLoc[ i ];
    }
}
////////////////////////////////////////////////////////////////////////////////
Wand::~Wand()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Wand::UpdateNavigation()
{
    //Remove the pointer if present
    if( beamGeode.valid() )
    {
        rootNode->asGroup()->removeChild( beamGeode.get() );
    }

    //If the wand does not exist
    if( wand->isStupefied() )
    {
        return;
    }

    m_buttonPushed = false;
    
    //Update the wand direction every frame
    UpdateWandLocalDirection();

    buttonData[ 1 ] = digital[ 1 ]->getData();
    buttonData[ 2 ] = digital[ 2 ]->getData();
    buttonData[ 4 ] = digital[ 4 ]->getData();

    m_rotIncrement.set( 0, 0, 0, 1 );
    osg::Quat world_quat = activeDCS->getAttitude();

    double* tempWorldTrans = activeDCS->GetVETranslationArray();
    m_worldTrans[ 0 ] = -tempWorldTrans[ 0 ];
    m_worldTrans[ 1 ] = -tempWorldTrans[ 1 ];
    m_worldTrans[ 2 ] = -tempWorldTrans[ 2 ];

    //This is NOT how we should do things
    //Command should allowed to be null but because we always
    //have to have a command due to our command structure
    //this hack must be in here
    //This should be changed once our complete command structure is in place
    std::string commandType;
    std::string newCommand;
    if( command )
    {
        commandType = command->GetCommandName();
    }
    else
    {
        commandType = "wait";
        newCommand = "wait";
    }

    if( !commandType.compare( "Navigation_Data" ) )
    {
        VE_XML::DataValuePairWeakPtr commandData = command->GetDataValuePair( 0 );
        cfdIso_value = commandData->GetDataValue();
        newCommand = commandData->GetDataName();
    }

    if( !newCommand.compare( "ROTATE_ABOUT_HEAD" ) )         
    {
        SetHeadRotationFlag( cfdIso_value );
    }
    else if( !newCommand.compare( "Z_ZERO_PLANE" ) )         
    {
        SetSubZeroFlag( cfdIso_value );
    }
    else if( !newCommand.compare( "RESET_NAVIGATION_POSITION" ) )         
    {
        for( unsigned int i = 0; i < 3; ++i )
        {
            m_worldTrans[ i ] = 0.0f;
            world_quat[ i ] = 0.0f;
            center_point->mData[ i ] = 0.0f;
        }

        world_quat[ 3 ] = 1.0f;
    }
    else if( !newCommand.compare( "CHANGE_TRANSLATION_STEP_SIZE" ) )         
    {
        //This equation returns a range of ~ 0.01' -> 220'
        //the equation is 1/100 * e ^ ( x / 10 ) where 1 < x < 100
        translationStepSize = 0.01f * exp( cfdIso_value * 0.10f );   
    }
    else if( !newCommand.compare( "CHANGE_ROTATION_STEP_SIZE" ) )         
    {
        //This equation returns a range of ~ 0.00029' -> 1.586' 
        //NOTE: These are in degrees
        //This equation is 1 / 750 * ( x / 2 ) ^ 2.2 where 1 < x < 50 
        rotationStepSize = 0.001333f * powf( ( cfdIso_value * 0.5f ), 2.2f );
    }

    //Navigate with buttons now
    if( ( buttonData[ 1 ] == gadget::Digital::TOGGLE_ON ) || 
        ( buttonData[ 1 ] == gadget::Digital::ON ) )
    {
        m_buttonPushed = true;
        RotateAboutWand();
    }
    //Navigation based on current wand direction
    else if( buttonData[ 2 ] == gadget::Digital::TOGGLE_ON ||
             buttonData[ 2 ] == gadget::Digital::ON )
    { 
        double* tempWandDir = GetDirection();
        vprDEBUG( vesDBG, 1 ) << "|\tWand direction :" 
            << tempWandDir[ 0 ] << " : "
            << tempWandDir[ 1 ] << " : " << tempWandDir[ 2 ]
            << std::endl << vprDEBUG_FLUSH;

        m_buttonPushed = true;
        for( int i = 0; i < 3; ++i )
        {
            //Update the translation movement for the objects
            //How much object should move
            m_worldTrans[ i ] += tempWandDir[ i ] * translationStepSize;
        }
    }
    //Reset back to 0, 0, 0
    else if( buttonData[ 4 ] == gadget::Digital::TOGGLE_ON ||
             buttonData[ 4 ] == gadget::Digital::ON )
    {         
        m_buttonPushed = true;
        for( unsigned int i = 0; i < 3; ++i )
        {
            m_worldTrans[ i ] = 0.0f;
            world_quat[ i ] = 0.0f;
            center_point->mData[ i ] = 0.0f;
        }
        
        world_quat[ 3 ] = 1.0f;
    }
    
    ///If we actually pushed a button then move things
    if( m_buttonPushed )
    {
        //Set the DCS postion based off of previous 
        //manipulation of the worldTrans array
        for( unsigned int i = 0; i < 3; ++i )
        {   
            m_worldTrans[ i ] = -m_worldTrans[ i ];
        }

        //Do not allow translation below z = 0 plane
        if( subzeroFlag )
        {
            if( m_worldTrans[ 2 ] > 0 )
            {
                m_worldTrans[ 2 ] = 0;
            }
        }

        activeDCS->SetTranslationArray( m_worldTrans );
        world_quat *= m_rotIncrement;
        activeDCS->SetQuat( world_quat );
    }

    vprDEBUG( vesDBG, 3 ) << "|\tEnd Navigate" << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void Wand::UpdateSelection()
{
    UpdateObjectHandler();
}
////////////////////////////////////////////////////////////////////////////////
void Wand::SetStartEndPoint( osg::Vec3d* startPoint, osg::Vec3d* endPoint )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Wand::SetHeadRotationFlag( int input )
{
    rotationFlag = input;
}
////////////////////////////////////////////////////////////////////////////////
void Wand::SetVECommand( VE_XML::Command* veCommand )
{
    command = veCommand;
}
////////////////////////////////////////////////////////////////////////////////
void Wand::SelectObject()
{
    osg::Vec3d startPoint, endPoint;
    SetupStartEndPoint( &startPoint, &endPoint );

    beamLineSegment->set( startPoint, endPoint );

    osgUtil::IntersectVisitor objectBeamIntersectVisitor;
    objectBeamIntersectVisitor.addLineSegment( beamLineSegment.get() );

    //Add the IntersectVisitor to the root Node so that all all geometry will be
    //checked and no transforms are done to the Line segement.
    rootNode->accept( objectBeamIntersectVisitor );

    osgUtil::IntersectVisitor::HitList beamHitList;
    beamHitList = objectBeamIntersectVisitor.getHitList( beamLineSegment.get() );

    ProcessHit( beamHitList );
}
////////////////////////////////////////////////////////////////////////////////
void Wand::ProcessHit( osgUtil::IntersectVisitor::HitList listOfHits )
{ 
    osgUtil::Hit objectHit;
    selectedGeometry = 0;

    if ( listOfHits.empty() )
    {
        vprDEBUG(vesDBG,1) << "|\tWand::ProcessHit No object selected" 
                           << std::endl << vprDEBUG_FLUSH;

        activeDCS = VE_SceneGraph::SceneManager::instance()->GetWorldDCS();

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
    VE_SceneGraph::FindParentsVisitor parentVisitor( selectedGeometry.get() );
    osg::ref_ptr< osg::Node > parentNode = parentVisitor.GetParentNode();
    if( parentNode.valid() )
    {
        vprDEBUG( vesDBG, 1 ) << "|\tObjects has name " 
            << parentNode->getName() << std::endl << vprDEBUG_FLUSH;

        vprDEBUG( vesDBG, 1 ) << "|\tObjects descriptors " 
            << parentNode->getDescriptions().at( 1 ) 
            << std::endl << vprDEBUG_FLUSH;

        activeDCS = dynamic_cast< VE_SceneGraph::DCS* >( parentNode.get() );
    }
    else
    {
        selectedGeometry = objectHit._geode;

        vprDEBUG( vesDBG, 1 ) << "|\tObject does not have name parent name " 
          << objectHit._geode->getParents().front()->getName() 
          << std::endl << vprDEBUG_FLUSH;

        activeDCS = VE_SceneGraph::SceneManager::instance()->GetWorldDCS();
    }
}
////////////////////////////////////////////////////////////////////////////////
//This function currently deletes the existing beam each time it is called and
//add a new beam.  This should be replaced such that it is only called once
//and then a transform is modified for the location.  
void Wand::DrawLine( osg::Vec3d start, osg::Vec3d end )
{   
    if( beamGeode.valid() )
    {
        rootNode->asGroup()->removeChild( beamGeode.get() );
    }

    beamGeode = new osg::Geode();
    beamGeometry = new osg::Geometry();
    beamGeode->addDrawable( beamGeometry.get() );
    beamGeode->setName( this->laserName );

    rootNode->asGroup()->addChild( beamGeode.get() );

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
        new osg::DrawElementsUInt( osg::PrimitiveSet::QUADS, 0);
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
void Wand::UpdateObjectHandler()
{
    vprDEBUG( vesDBG, 3 ) << "|\tStart Wand::UpdateObjectHandler" 
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

    vprDEBUG( vesDBG, 3 ) << "|\tEnd Wand::UpdateObjectHandler" 
                          << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void Wand::SetupStartEndPoint( osg::Vec3d * startPoint, osg::Vec3d * endPoint )
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
void Wand::TranslateObject()
{
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
void Wand::UpdateWandLocalDirection()
{
    //Get the normalized direction relative to the juggler frame
    gmtl::Vec3d vjVec;
    vjVec.set( 0.0f, 0.0f, -1.0f );
    Matrix44d vjMat = convertTo< double >( wand->getData() );

    gmtl::xform( vjVec, vjMat, vjVec );
    gmtl::normalize( vjVec );

    //Transform from juggler to osg...
    dir[0] =  vjVec[ 0 ];
    dir[1] = -vjVec[ 2 ];
    dir[2] =  vjVec[ 1 ];
}
////////////////////////////////////////////////////////////////////////////////
void Wand::UpdateWandGlobalLocation()
{
    //Transform wand point into global space get juggler Matrix of worldDCS
    //Note:: for osg we are in z up land
    gmtl::Point3d loc_temp, osgPointLoc;
    Matrix44d vjMat = convertTo< double >( wand->getData() );

    gmtl::setTrans( loc_temp, vjMat );
    osgPointLoc[ 0 ] =  loc_temp[ 0 ];
    osgPointLoc[ 1 ] = -loc_temp[ 2 ];
    osgPointLoc[ 2 ] =  loc_temp[ 1 ];
   
    for ( size_t i = 0; i < 3; ++i )
    {
        LastWandPosition[ i ] = objLoc[ i ];
        //cursorLoc[ i ] = this->loc[ i ];// + this->dir[ i ] * this->cursorLen;
        objLoc[ i ] = osgPointLoc[ i ];
    }
}
////////////////////////////////////////////////////////////////////////////////
double* Wand::GetObjLocation()
{
    return objLoc;
}
////////////////////////////////////////////////////////////////////////////////
double* Wand::GetDirection()
{
    return dir;
}
////////////////////////////////////////////////////////////////////////////////
void Wand::UpdateDeltaWandPosition()
{
    for ( size_t i = 0; i < 3; ++i )
    {
        deltaTrans[ i ] = objLoc[ i ] - LastWandPosition[ i ];
    }
}
////////////////////////////////////////////////////////////////////////////////
void Wand::SetSubZeroFlag( int input )
{
    subzeroFlag = input;
}
////////////////////////////////////////////////////////////////////////////////
void Wand::RotateAboutWand()
{
    gmtl::Matrix44d vrjWandMat = convertTo< double >( wand->getData() );
    gmtl::Quatd wandQuat = gmtl::make< gmtl::Quatd >( vrjWandMat );
    
    osg::Vec3d tempVec( 0, 0, wandQuat[ 1 ] );
    m_rotIncrement = 
        osg::Quat( osg::DegreesToRadians( -rotationStepSize ), tempVec );
    
    ///If we are rotating about the users position
    if( !rotationFlag )
    {
        return;
    }
    
    vjHeadMat = convertTo< double >( head->getData() );
    
    //Get juggler Matrix of worldDCS
    //Note:: for pf we are in juggler land
    //       for osg we are in z up land
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
    tempVec.set( wandQuat[ 0 ], -wandQuat[ 2 ], wandQuat[ 1 ] );
    m_rotIncrement = 
        osg::Quat( osg::DegreesToRadians( -rotationStepSize ), tempVec );
    
    gmtl::Matrix44d rotMatTemp = gmtl::makeRot< gmtl::Matrix44d >
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
