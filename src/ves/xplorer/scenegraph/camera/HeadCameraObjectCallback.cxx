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
#include <ves/xplorer/scenegraph/camera/HeadCameraObjectCallback.h>
#include <ves/xplorer/scenegraph/camera/CameraObject.h>

#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/scenegraph/SceneManager.h>

#include <gmtl/gmtl.h>

// --- OSG Includes --- //
#include <osg/Camera>
#include <osg/Light>

using namespace ves::xplorer::scenegraph::camera;

////////////////////////////////////////////////////////////////////////////////
HeadCameraObjectCallback::HeadCameraObjectCallback()
    :
    osg::Object(),
    osg::NodeCallback(),
    m_dcsMatrix()
{
    //Make it something other than identity
    m_dcsMatrix( 0, 0 ) = 0.0;
}
////////////////////////////////////////////////////////////////////////////////
HeadCameraObjectCallback::HeadCameraObjectCallback( const HeadCameraObjectCallback& input )
    :
    osg::Object( input ),
    osg::NodeCallback( input ),
    m_dcsMatrix( input.m_dcsMatrix )
{
    if( &input != this )
    {
        ;
    }
}
////////////////////////////////////////////////////////////////////////////////
HeadCameraObjectCallback::~HeadCameraObjectCallback()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void HeadCameraObjectCallback::operator()( osg::Node* node, osg::NodeVisitor* )
{
    osg::ref_ptr< CameraObject > cameraObject =
        static_cast< CameraObject* >( node );

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
    
    osg::Matrixd tempMatrix( dcs.GetMat().getData() );
    if( tempMatrix != m_dcsMatrix )
    {
        m_dcsMatrix = tempMatrix;

        /*
        osg::Light& light = cameraObject->GetLight();
        osg::Vec4 position = light.getPosition() * tempMatrix;
        osg::Vec3 direction = light.getDirection() * tempMatrix;
        light.setPosition( position );
        light.setDirection( direction );
        */

        tempMatrix =
            osg::Matrixd::inverse( tempMatrix ) *
            cameraObject->GetInitialViewMatrix();

        osg::Camera& camera = cameraObject->GetCamera();
        camera.setViewMatrix( tempMatrix );

        cameraObject->CalculateMatrixMVPT();
    }
}
////////////////////////////////////////////////////////////////////////////////
