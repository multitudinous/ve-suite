/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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
    t( 0.0f ),
    movementIntervalCalc( 0.01 ),
    m_movementSpeed( 10.0f ),
    m_frameTimer( new vpr::Timer() ),
    mBeginAnim( false ),
    mSetCenterPoint( false ),
    mCenterPointDCS( 0 )
{
    flyThroughList.clear();
    completionTest.push_back( 0 );

    mEventHandlers[ std::string( "QC_LOAD_STORED_POINTS" ) ] =
        new ves::xplorer::event::QuatCamLoadFileEventHandler();
    mEventHandlers[ std::string( "QC_CLEAR_QUAT_DATA" ) ] =
        new ves::xplorer::event::QuatCamClearDataEventHandler();
}
////////////////////////////////////////////////////////////////////////////////
NavigationAnimationEngine::~NavigationAnimationEngine()
{
    delete m_frameTimer;
}
////////////////////////////////////////////////////////////////////////////////
void NavigationAnimationEngine::SetDCS( ves::xplorer::scenegraph::DCS* worldDCS )
{
    _worldDCS = worldDCS;
}

////////////////////////////////////////////////////////////////////////////////
void NavigationAnimationEngine::ProcessCommand()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void NavigationAnimationEngine::SetAnimationSpeed( double travelSpeed )
{
    m_movementSpeed = travelSpeed;
}
////////////////////////////////////////////////////////////////////////////////
void NavigationAnimationEngine::IncrementAnimationSpeed( double increment )
{
    m_movementSpeed += increment;
    //We can never have a negative speed
    if( m_movementSpeed < 0.0 )
    {
        m_movementSpeed = 0.1;
    }
}
////////////////////////////////////////////////////////////////////////////////
void NavigationAnimationEngine::PreFrameUpdate()
{
    m_frameTimer->stopTiming();
    if( !mBeginAnim )
    {
        m_deltaTime = m_frameTimer->getTiming();
        m_frameTimer->reset();
        m_frameTimer->startTiming();
        return;
    }
    m_deltaTime = m_frameTimer->getTiming();

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

    //See if we only have a small distance left to travel. If so then lets 
    //exit the animation
    gmtl::Vec3d deltaLeft = mEndVec - curVec;
    float length = gmtl::length( deltaLeft );
    gmtl::AxisAngled currentAngle;
    gmtl::set( currentAngle, tempQuat );
    double angle = currentAngle.getAngle();
    double deltaAngle = gmtl::Math::abs( m_lastAngle - angle );
    m_lastAngle = angle;
    if( deltaAngle < 0.01 && length < 0.01 )
    {
        ///Override what was determined earlier because we know we do
        ///not have to move that far.
        t = 1.0f;
        mBeginAnim = false;
    }
    else
    {
        t += movementIntervalCalc;
        if( t >= ( 1.0f - movementIntervalCalc ) )
        {
            t = 1.0f;
            mBeginAnim = false;
        }
    }
    //interpolate the rotation and translation
    gmtl::lerp( tempVec, t, curVec, mEndVec );
    gmtl::slerp( tempResQuat, t, tempQuat, mEndQuat );
    
    //convert gmtl vec to double *
    double tempConvVec[3] ;
    tempConvVec[0] = tempVec[0];
    tempConvVec[1] = tempVec[1];
    tempConvVec[2] = tempVec[2];

    _worldDCS->SetTranslationArray( tempConvVec );

    //convert gmtl quat to osg quat
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
    
    m_frameTimer->reset();
    m_frameTimer->startTiming();
}
////////////////////////////////////////////////////////////////////////////////
void NavigationAnimationEngine::UpdateCommand()
{
    std::cout << "|\tNavigationAnimationEngine::UpdateCommand doing nothing "
              << std::endl;
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
    m_lastAngle = 0.0;
    mSetCenterPoint = setCenterPoint;
    mCenterPointDCS = centerPointDCS;
    
    //Set up the interval constant
    gmtl::Vec3d curVec;
    //osg vec to gmtl vec
    double* temp = _worldDCS->GetVETranslationArray();
    for( int i = 0; i < 3; ++i )
    {
        curVec[ i ] = temp[ i ];
    }
    gmtl::Vec3d deltaLeft = mEndVec - curVec;
    float length = gmtl::length( deltaLeft );
    //movementIntervalCalc =
    //    1.0 / ( length / ( m_movementSpeed * m_deltaTime ) );

    double timeConstant = length/m_movementSpeed;
    double numSegments = timeConstant/m_deltaTime;
    movementIntervalCalc = 1.0/numSegments;
    //std::cout << numSegments << " " <<  movementIntervalCalc << std::endl;
    if( (length == 0) || (movementIntervalCalc > 1.0) || (numSegments < 2.0) )
    {
        movementIntervalCalc = 0.01;
    }
    //std::cout << length << " " << m_movementSpeed << " " << m_deltaTime << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
bool NavigationAnimationEngine::IsActive()
{
    return mBeginAnim;
}
////////////////////////////////////////////////////////////////////////////////
