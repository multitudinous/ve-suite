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

// --- VE-Suite Includes --- //
#include <ves/xplorer/CommandHandler.h>    //This needs to be first
#include <ves/xplorer/Debug.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/DeviceHandler.h>
#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/CoordinateSystemTransform.h>

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
#include <gmtl/gmtl.h>
#include <gmtl/Quat.h>
#include <gmtl/Xforms.h>
#include <gmtl/Generate.h>

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

    mBeginAnim = false;
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
/*void NavigationAnimationEngine::ClearQuaternionData()
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
}*/
////////////////////////////////////////////////////////////////////////////////
/*void NavigationAnimationEngine::Relocate(
ves::xplorer::scenegraph::DCS* worldDCS )
{
    gmtl::Matrix44d vjm;

    double temp = GetQuatCamIncrementor();

    if( t >= 1.0f )
    {
        activecam = false;
        t = 0.0f;
    }
}*/
////////////////////////////////////////////////////////////////////////////////
void NavigationAnimationEngine::ProcessCommand()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void NavigationAnimationEngine::PreFrameUpdate()
{
    /*if( activecam )
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

        //
    }*/
    if( mBeginAnim )
    {
        //optimize
        movementIntervalCalc = 0.01;
            //1 / ( vecDistance / ( movementSpeed * frameTimer->getTiming() ));

        //change
        GetQuatCamIncrementor();

        gmtl::Quatd tempResQuat;
        gmtl::Vec3d tempVec;
        gmtl::Vec3d curVec;

        //osg vec to gmtl vec
        double* temp = _worldDCS->GetVETranslationArray();
        for( int i = 0; i < 3; ++i )
        {
            curVec[ i ] = temp[ i ];
        }
        
        //convert osg quat to gmtl quat
        osg::Quat tempWorldQuat = _worldDCS->GetQuat();
        gmtl::Quatd tempQuat( tempWorldQuat[0], tempWorldQuat[1],
            tempWorldQuat[2], tempWorldQuat[3] );

        //interpolate the rotation and translation
        gmtl::lerp( tempVec, t, curVec, mEndVec );
        gmtl::slerp( tempResQuat, t, tempQuat, mEndQuat );
        
        //convert gmtl vec to double *
        double tempConvVec[3] ;
        tempConvVec[0] = tempVec[0];
        tempConvVec[1] = tempVec[1];
        tempConvVec[2] = tempVec[2];

        //gmtl::Quatd tempRotQuat = gmtl::makeRot< gmtl::Quatd >( curVec, tempVec );
        //tempQuat = tempRotQuat * tempQuat;

        _worldDCS->SetTranslationArray( tempConvVec );

        //convert gmtl quat to osg quat
        //osg::Quat tempOSGQuat(
        //    mEndQuat[0], mEndQuat[1], mEndQuat[2], mEndQuat[3] );
        osg::Quat tempOSGQuat(
            tempResQuat[0], tempResQuat[1], tempResQuat[2], tempResQuat[3] );
        
        //rotate and translate
        _worldDCS->SetQuat( tempOSGQuat );
        if( mSetCenterPoint == true && !mBeginAnim )
        {
            //Move the center point to the center of the selected object
            osg::ref_ptr< ves::xplorer::scenegraph::CoordinateSystemTransform > cst =
                new ves::xplorer::scenegraph::CoordinateSystemTransform(
                ves::xplorer::scenegraph::SceneManager::instance()->GetActiveSwitchNode(),
                    mCenterPointDCS, true );
            gmtl::Matrix44d localToWorldMatrix =
                cst->GetTransformationMatrix( false );

            //Multiplying by the new local matrix mCenterPoint
            osg::Matrixd tempMatrix;
            tempMatrix.set( localToWorldMatrix.getData() );
            osg::Vec3d center =
                mCenterPointDCS->getBound().center() * tempMatrix;
            gmtl::Point3d tempCenter( center.x(), center.y(), center.z() );
            ves::xplorer::DeviceHandler::instance()->
                SetCenterPoint( &tempCenter );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void NavigationAnimationEngine::UpdateCommand()
{
    std::cerr << "doing nothing in cfdQuatCamHandler::UpdateCommand()"
              << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
/*double NavigationAnimationEngine::getLinearDistance(
    gmtl::Vec3d vjVecLast, gmtl::Vec3d vjVecNext )
{
    double distance;
    gmtl::Vec3d temp;

    temp = vjVecNext - vjVecLast;

    distance = gmtl::length( temp );

    return distance;
}*/
////////////////////////////////////////////////////////////////////////////////
double NavigationAnimationEngine::GetQuatCamIncrementor()
{
    ////////////////////////////////////////////////////////////////////////
    //When in cluster mode this function is only called by the Master Node
    ////////////////////////////////////////////////////////////////////////

    if( t < ( 1.0f - movementIntervalCalc ) )
    {
        t += movementIntervalCalc;
    }
    else if( t >= ( 1.0f - movementIntervalCalc ) )
    {
        t = 1.0f;
        mBeginAnim = false;
    }

    return t;
}
////////////////////////////////////////////////////////////////////////////////
void NavigationAnimationEngine::SetAnimationEndPoints(
    gmtl::Vec3d navToPoint, gmtl::Quatd rotationPoint,
    bool setCenterPoint, ves::xplorer::scenegraph::DCS* centerPointDCS)
{
    mBeginAnim = true;
    mEndVec = navToPoint;
    mEndQuat = rotationPoint;
    t = 0.0f;
    mSetCenterPoint = setCenterPoint;
    mCenterPointDCS = centerPointDCS;
}
///////////////////////////////////////////////////////////////////////////////
bool NavigationAnimationEngine::IsActive()
{
    return mBeginAnim;
}