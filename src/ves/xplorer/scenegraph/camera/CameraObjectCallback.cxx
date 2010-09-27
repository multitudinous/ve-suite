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
#include <ves/xplorer/scenegraph/camera/CameraObjectCallback.h>
#include <ves/xplorer/scenegraph/camera/CameraObject.h>

#include <ves/xplorer/scenegraph/DCS.h>

// --- OSG Includes --- //
#include <osg/Camera>
#include <osg/Light>

using namespace ves::xplorer::scenegraph::camera;

////////////////////////////////////////////////////////////////////////////////
CameraObjectCallback::CameraObjectCallback()
    :
    osg::Object(),
    osg::NodeCallback(),
    m_dcsMatrix()
{
    //Make it something other than identity
    m_dcsMatrix( 0, 0 ) = 0.0;
}
////////////////////////////////////////////////////////////////////////////////
CameraObjectCallback::CameraObjectCallback( const CameraObjectCallback& input )
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
CameraObjectCallback::~CameraObjectCallback()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CameraObjectCallback::operator()( osg::Node* node, osg::NodeVisitor* nv )
{
    osg::ref_ptr< CameraObject > cameraObject =
        dynamic_cast< CameraObject* >( node );

    if( !cameraObject.valid() )
    {
        traverse( node, nv );
        return;
    }
    
    osg::Matrixd tempMatrix( cameraObject->GetDCS().GetMat().getData() );
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
