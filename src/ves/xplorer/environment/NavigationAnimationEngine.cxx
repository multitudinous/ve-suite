/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
#include <ves/xplorer/CommandHandler.h>    //This needs to be first
#include <ves/xplorer/Debug.h>
#include <ves/xplorer/ModelHandler.h>

#include <ves/xplorer/util/fileIO.h>

#include <ves/xplorer/environment/cfdEnum.h>
#include <ves/xplorer/environment/cfdQuatCam.h>
#include <ves/xplorer/environment/NavigationAnimationEngine.h>

#include <ves/xplorer/event/environment/QCClearDataEH.h>
#include <ves/xplorer/event/environment/QCLoadFileEH.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/OneDIntArray.h>

// --- vrJuggler Includes --- //
#include <vpr/System.h>

#include <boost/filesystem/operations.hpp> //includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>

#include <gmtl/Math.h>
#include <gmtl/Matrix.h>

// --- C/C++ Libraries --- //
#include <cmath>
#include <iostream>
#include <cstdlib>
#include <fstream>
#include <ostream>
#include <string>

using namespace ves::xplorer;

vprSingletonImp( NavigationAnimationEngine );

////////////////////////////////////////////////////////////////////////////////
NavigationAnimationEngine::NavigationAnimationEngine()
    :
    pointCounter( 0 ),
    movementIntervalCalc( 0.01 ),
    movementSpeed( 10.0f ),
    lastCommandId( 0 ),
    currentFrame( 0 ),
    writeFrame( 0 ),
    thisQuatCam( 0 ),
    t( 0.0f ),
    numQuatCams( 0 ),
    numPointsInFlyThrough( 0 ),
    activecam( false ),
    _runFlyThrough( false ),
    writeReadComplete( false ),
    cam_id( 0 ),
    activeFlyThrough( -1 ),
    quatCamDirName( "./" )
{
    flyThroughList.clear();
    completionTest.push_back( 0 );
    frameTimer = new vpr::Timer();

    quatCamFileName = "stored_viewpts_flythroughs.vel";

    mEventHandlers[ std::string( "QC_LOAD_STORED_POINTS" ) ] =
        new ves::xplorer::event::QuatCamLoadFileEventHandler();
    mEventHandlers[ std::string( "QC_CLEAR_QUAT_DATA" ) ] =
        new ves::xplorer::event::QuatCamClearDataEventHandler();
}
////////////////////////////////////////////////////////////////////////////////
NavigationAnimationEngine::~NavigationAnimationEngine()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void NavigationAnimationEngine::SetDCS( ves::xplorer::scenegraph::DCS* worldDCS )
{
    _worldDCS = worldDCS;
}
////////////////////////////////////////////////////////////////////////////////
void NavigationAnimationEngine::ClearQuaternionData()
{
    TurnOffMovement();

    for( size_t i = 0; i < QuatCams.size(); ++i )
    {
        delete QuatCams.at( i );
    }
    QuatCams.clear();

    flyThroughList.clear();

    //More hacking to initialize the flythroughlist
    //This forces us to only have one flythrought per ves file
    if( flyThroughList.empty() )
    {
        //AddNewFlythrough();
    }

    UpdateViewGUIPointData();
}
////////////////////////////////////////////////////////////////////////////////
/*void NavigationAnimationEngine::Relocate( ves::xplorer::scenegraph::DCS* worldDCS )
{
    gmtl::Matrix44d vjm;

    if( t == 0.0f )
    {
        QuatCams.at( cam_id )->SetCamPos(
            worldDCS->GetVETranslationArray(), worldDCS );
    }
    double temp = GetQuatCamIncrementor();

    if (( t < 1.0f ) )
    {
        QuatCams.at( cam_id )->MoveCam( temp );
        QuatCams.at( cam_id )->UpdateRotation( worldDCS );
    }
    else
    {
        activecam = false;
        t = 0.0f;
    }
}*/
////////////////////////////////////////////////////////////////////////////////
void NavigationAnimationEngine::TurnOffMovement()
{
    _runFlyThrough = false;
    activecam = false;
    pointCounter = 0;
    t = 0.0f;
}
////////////////////////////////////////////////////////////////////////////////
void NavigationAnimationEngine::ProcessCommand()
{
    if( !onMasterNode )
    {
        return;
    }

    bool flag = false;
    std::string commandType;
    if( veCommand )
    {
        commandType = veCommand->GetCommandName();
    }
    else
    {
        commandType = "wait";
    }

    if( !commandType.compare( "ViewLoc_Data" ) )
    {
        ves::open::xml::DataValuePairPtr commandData =
            veCommand->GetDataValuePair( 0 );

        //Change this to grab a OneDIntArray
        //via GetDataXMLObject() from DataValuePair---biv
        std::vector< double > commandIds;
        commandData->GetData( commandIds );
        std::string newCommand = commandData->GetDataName();
        if( !newCommand.compare( "LOAD_NEW_VIEWPT" ) )
        {
            writeFrame = currentFrame;
            TurnOffMovement();
            //AddViewPtToFlyThrough( 0, QuatCams.size() );
            //LoadData( _worldDCS.get() );
            //WriteToFile( quatCamFileName );
            writeReadComplete = true;
            lastCommandId = LOAD_NEW_VIEWPT;
            flag = true;
        }
        else if( !newCommand.compare( "MOVE_TO_SELECTED_LOCATION" ) )
        {
            //frameTimer->startTiming();
            activecam = true;
            cam_id = static_cast< unsigned int >( commandIds.at( 0 ) );
            flag = true;
        }
        else if( !newCommand.compare( "REMOVE_SELECTED_VIEWPT" ) )
        {
            writeFrame = currentFrame;
            TurnOffMovement();
            cam_id = static_cast< unsigned int >( commandIds.at( 0 ) );
            //RemoveViewPt();
            //WriteToFile( quatCamFileName );
            writeReadComplete = true;
            lastCommandId = REMOVE_SELECTED_VIEWPT;
            flag = true;
        }
        else if( !newCommand.compare( "ADD_NEW_POINT_TO_FLYTHROUGH" ) )
        {
            writeFrame = currentFrame;
            TurnOffMovement();
            //AddViewPtToFlyThrough(
            //    static_cast< unsigned int >( commandIds.at( 0 ) ),
            //    static_cast< unsigned int >( commandIds.at( 1 ) ) );
            //WriteToFile( quatCamFileName );
            writeReadComplete = true;
            lastCommandId = ADD_NEW_POINT_TO_FLYTHROUGH;
            flag = true;
        }
        else if( !newCommand.compare( "INSERT_NEW_POINT_IN_FLYTHROUGH" ) )
        {
            writeFrame = currentFrame;
            //TurnOffMovement();
            //InsertViewPtInFlyThrough(
            //    static_cast< unsigned int >( commandIds.at( 0 ) ),
            //    static_cast< unsigned int >( commandIds.at( 1 ) ),
            //    static_cast< unsigned int >( commandIds.at( 2 ) ) );
            //WriteToFile( quatCamFileName );
            writeReadComplete = true;
            lastCommandId = INSERT_NEW_POINT_IN_FLYTHROUGH;
            flag = true;
        }
        else if( !newCommand.compare( "REMOVE_POINT_FROM_FLYTHROUGH" ) )
        {
            writeFrame = currentFrame;
            TurnOffMovement();
            //RemoveFlythroughPt(
            //    static_cast< unsigned int >( commandIds.at( 0 ) ),
            //    static_cast< unsigned int >( commandIds.at( 0 ) ) );
            //WriteToFile( quatCamFileName );
            writeReadComplete = true;
            lastCommandId = REMOVE_POINT_FROM_FLYTHROUGH;
            flag = true;
        }
        else if( !newCommand.compare( "DELETE_ENTIRE_FLYTHROUGH" ) )
        {
            writeFrame = currentFrame;
            TurnOffMovement();
            //DeleteEntireFlythrough(
            //    static_cast< unsigned int >( commandIds.at( 0 ) ) );
            //WriteToFile( quatCamFileName );
            writeReadComplete = true;
            lastCommandId = DELETE_ENTIRE_FLYTHROUGH;
            flag = true;
        }
        else if( !newCommand.compare( "ADD_NEW_FLYTHROUGH" ) )
        {
            writeFrame = currentFrame;
            TurnOffMovement();
            //AddNewFlythrough();
            //WriteToFile( quatCamFileName );
            writeReadComplete = true;
            lastCommandId = ADD_NEW_FLYTHROUGH;
            flag = true;
        }
        else if( !newCommand.compare( "RUN_ACTIVE_FLYTHROUGH" ) )
        {
            activeFlyThrough =
                static_cast< unsigned int >( commandIds.at( 0 ) );
            _runFlyThrough = true;
            activecam = true;
            flag = true;
        }
        else if( !newCommand.compare( "STOP_ACTIVE_FLYTHROUGH" ) )
        {
            TurnOffMovement();
            flag = true;
        }
        else if( !newCommand.compare( "CHANGE_MOVEMENT_SPEED" ) )
        {
            movementSpeed = commandIds.at( 0 );
            flag = true;
        }

        if( flag )
        {
            UpdateViewGUIPointData();
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void NavigationAnimationEngine::UpdateViewGUIPointData()
{
    ves::open::xml::CommandPtr viewPointGUIData(
        new ves::open::xml::Command() );
    viewPointGUIData->SetCommandName( "VIEWPOINT_GUI_DATA" );
    size_t nViewPoints = QuatCams.size( );

    //This will need to change once we can modify viewpoint names
    std::stringstream name;
    for( size_t i = 0; i < nViewPoints; ++i )
    {
        name << "View Location_" << i;
        ves::open::xml::DataValuePairPtr viewPointNames(
            new ves::open::xml::DataValuePair( ) );
        viewPointNames->SetData( "View Location", name.str() );
        viewPointGUIData->AddDataValuePair( viewPointNames );
        name.clear( );
    }
    ves::xplorer::CommandHandler::instance()->SetXMLCommand( viewPointGUIData );
}
////////////////////////////////////////////////////////////////////////////////
void NavigationAnimationEngine::PreFrameUpdate()
{
    //If a quat is active this will move the cam to the next location
    currentFrame += 1;
    ves::open::xml::CommandPtr tempCommand = 
        ModelHandler::instance()->GetXMLCommand();
    if( tempCommand )
    {
        vprDEBUG( vesDBG, 3 )
            << "|\tcfdQuatCamHandler::PreFrameUpdate Command Name : "
            << tempCommand->GetCommandName()
            << std::endl << vprDEBUG_FLUSH;
        std::map< std::string, ves::xplorer::event::EventHandler* >::iterator
            currentEventHandler;
        currentEventHandler = mEventHandlers.find(
            tempCommand->GetCommandName() );
        if( currentEventHandler != mEventHandlers.end() )
        {
            vprDEBUG( vesDBG, 1 )
                << "|\t cfdQuatCamHandler::PreFrameUpdate Executing: "
                << tempCommand->GetCommandName()
                << std::endl << vprDEBUG_FLUSH;
            currentEventHandler->second->SetGlobalBaseObject();
            currentEventHandler->second->Execute( tempCommand );
        }
        else
        {
            ProcessCommand();
        }
    }
    frameTimer->stopTiming();
    if( _runFlyThrough )
    {
        if( !activecam )
        {
            activecam = true;
            if( pointCounter < static_cast< unsigned int >(
                    flyThroughList.at( activeFlyThrough ).size() ) - 1 )
            {
                pointCounter += 1;
            }
            else
            {
                pointCounter = 0;
            }
            //pointCounter = ( pointCounter == ( ( int )flyThroughList.at(
                //activeFlyThrough ).size() - 1 ) ) ? 0 : ++pointCounter;
        }
        cam_id = flyThroughList.at( activeFlyThrough ).at( pointCounter );
    }

    if( activecam )
    {
        double vecDistance;

        if( t == 0.0f )
        {
            gmtl::Vec3d vjVecTemp;
            double* veTransTemp = _worldDCS->GetVETranslationArray();
            for( int i = 0; i < 3; ++i )
            {
                vjVecTemp[ i ] = veTransTemp[ i ];
            }
            vecDistance = getLinearDistance(
                vjVecTemp, QuatCams.at( cam_id )->GetTrans() );
        }
        else
        {
            vecDistance = getLinearDistance(
                QuatCams.at( cam_id )->GetLastTrans(),
                QuatCams.at( cam_id )->GetTrans() );
        }

        movementIntervalCalc =
            1 / ( vecDistance / ( movementSpeed * frameTimer->getTiming() ) );

        //Relocate( _worldDCS.get() );
    }

    frameTimer->reset();
    frameTimer->startTiming();
}
////////////////////////////////////////////////////////////////////////////////
void NavigationAnimationEngine::UpdateCommand()
{
    std::cerr << "doing nothing in cfdQuatCamHandler::UpdateCommand()"
              << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
double NavigationAnimationEngine::getLinearDistance(
    gmtl::Vec3d vjVecLast, gmtl::Vec3d vjVecNext )
{
    double distance;
    gmtl::Vec3d temp;

    temp = vjVecNext - vjVecLast;

    distance = gmtl::length( temp );

    return distance;
}
////////////////////////////////////////////////////////////////////////////////
double NavigationAnimationEngine::GetQuatCamIncrementor()
{
    ////////////////////////////////////////////////////////////////////////
    //When in cluster mode this function is only called by the Master Node
    ////////////////////////////////////////////////////////////////////////

    if( ( t < 1.0f ) && ( t < ( 1.0f - movementIntervalCalc ) ) )
    {
        t += movementIntervalCalc;
    }
    else if( ( t < 1.0f ) && ( t >= ( 1.0f - movementIntervalCalc ) ) )
    {
        t = 1.0f;
    }

    return t;
}
////////////////////////////////////////////////////////////////////////////////
bool NavigationAnimationEngine::IsActive()
{
    return activecam;
}
////////////////////////////////////////////////////////////////////////////////
void NavigationAnimationEngine::SetMasterNode( bool masterNode )
{
    onMasterNode = masterNode;
}
////////////////////////////////////////////////////////////////////////////////
