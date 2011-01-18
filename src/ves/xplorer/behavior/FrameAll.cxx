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
#include <ves/xplorer/behavior/FrameAll.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/scenegraph/GLTransformInfo.h>

#include <ves/xplorer/EnvironmentHandler.h>

#include <ves/xplorer/environment/cfdDisplaySettings.h>

#include <ves/xplorer/eventmanager/EventManager.h>

#include <osg/BoundingSphere>
#include <osg/Vec3d>
#include <osg/Matrix>
#include <osg/Group>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/LineWidth>
#include <osg/AutoTransform>
#include <osg/io_utils>


#include <gmtl/Matrix.h>
#include <gmtl/AxisAngle.h>
#include <gmtl/Generate.h>
#include <gmtl/Misc/MatrixConvert.h>

namespace ves
{
namespace xplorer
{
namespace behavior
{

FrameAll::FrameAll()
{
    mHead.init( "VJHead" );

    CONNECTSIGNALS_0( "%FrameAll%", void(), &FrameAll::DoFrameAll,
                      mConnections, any_SignalType, normal_Priority );
}

FrameAll::~FrameAll()
{
    ;
}

void FrameAll::DoFrameAll()
{

    scenegraph::SceneManager& m_sceneManager =
            *( ves::xplorer::scenegraph::SceneManager::instance() );

    osg::Group* activeSwitchNode = m_sceneManager.GetActiveSwitchNode();
    scenegraph::DCS* activeNavSwitchNode = m_sceneManager.GetActiveNavSwitchNode();
    osg::BoundingSphere bs = activeSwitchNode->computeBound();

    //Meters to feet conversion
    double m2ft = 3.2808399;

    //Set the start point to be the head position in osg space
    osg::Vec3d startPoint( 0.0, 0.0, 0.0 );
    {
        //Note: for osg we are in z up land
        gmtl::Matrix44d vjHeadMat =
            gmtl::convertTo< double >( mHead->getData() );
        gmtl::Point3d jugglerHeadPoint =
            gmtl::makeTrans< gmtl::Point3d >( vjHeadMat );

        //We have to offset negative m_currX because the
        //view and frustum are drawn for the left eye
        startPoint.set(
            jugglerHeadPoint.mData[ 0 ] - ( 0.0345 * m2ft ),
           -jugglerHeadPoint.mData[ 2 ],
            jugglerHeadPoint.mData[ 1 ] );
    }


    // Get the screen corner values from EnvironmentHandler
    ves::xplorer::cfdDisplaySettings* displaySettings =
            ves::xplorer::EnvironmentHandler::instance()->GetDisplaySettings();
    std::map< std::string, double > screenCornerValues =
            displaySettings->GetScreenCornerValues();
    double mXMinScreen = screenCornerValues[ "xmin" ];
    double mXMaxScreen = screenCornerValues[ "xmax" ];
    double mYMinScreen = screenCornerValues[ "ymin" ];
    double mYMaxScreen = screenCornerValues[ "ymax" ];
    double mZValScreen = screenCornerValues[ "zval" ];

    // Get window dimensions
    std::pair< int, int > screenDims = displaySettings->GetScreenResolution();
    unsigned int m_windowWidth = screenDims.first;
    unsigned int m_windowHeight = screenDims.second;

    double mAspectRatio =
            static_cast< double >( m_windowWidth ) / static_cast< double >( m_windowHeight );

    double mFoVZ = m_sceneManager.GetCurrentGLTransformInfo()->GetFOVZ();
std::cout << mFoVZ << std::endl;
    //Set the end point
    osg::Vec3d endPoint( 0.0, 0.0, 0.0 );
    {
        //Be sure m_windowWidth and m_windowHeight are set before calling this function
        double xScreenRatio =
            ( mXMaxScreen - mXMinScreen ) / static_cast< double >( m_windowWidth );
        double yScreenRatio =
            ( mYMaxScreen - mYMinScreen ) / static_cast< double >( m_windowHeight );

        //Get the center screen position in juggler world coordinates
        osg::Vec3d vrjCenterScreenPosition(
            mXMinScreen + 0.5 * static_cast< double >( m_windowWidth ) * xScreenRatio,
            mYMinScreen + 0.5 * static_cast< double >( m_windowHeight ) * yScreenRatio,
            mZValScreen );

        //Convert meters to feet
        vrjCenterScreenPosition *= m2ft;

        //Get the screen position in osg world coordinates
        osg::Vec3d centerPosition(
            vrjCenterScreenPosition.x(),
           -vrjCenterScreenPosition.z(),
            vrjCenterScreenPosition.y() );

        //Calculate the distance we need to move along the center vector to fit
        //the bounding sphere of all the geometry inside the viewing frustum
        double theta = mFoVZ * 0.5;
        if( mAspectRatio < 1.0 )
        {
            theta *= mAspectRatio;
        }
        double distance;
        distance = ( bs.radius() / tan( theta ) );

        //Now we can get our center frustum vector
        osg::Vec3d vecNear = centerPosition - startPoint;
        osg::Vec3d vecFar = -startPoint;
        vecFar.y() = distance + vecFar.y();

        double ratio = vecFar.y() / vecNear.y();
        endPoint.set(
            startPoint.x() + ( vecNear.x() * ratio ),
            distance,
            startPoint.z() + ( vecNear.z() * ratio ) );
    }

    //Set the current switch node's matrix w/ the new "frame all" transform
    activeNavSwitchNode->setPosition( endPoint - bs.center() );
    activeNavSwitchNode->setAttitude( osg::Quat( 0.0, 0.0, 0.0, 1.0 ) );

    // Appears to be unnecessary since this function does not use the center point.
    // I think this is legacy from KeyboardMouse's handling of all the nav code. --RPT

    //Get the new center of the bounding sphere in camera space
//    activeSwitchNode->computeBound();
//    osg::Vec3d center =
//        bs.center() * osg::Matrixd( activeNavSwitchNode->GetMat().getData() );
//
//
//    mCenterPoint->set( center.x(), center.y(), center.z() );

}

}
}
}
