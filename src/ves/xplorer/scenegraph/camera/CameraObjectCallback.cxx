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

// --- My Includes --- //
#include <ves/xplorer/scenegraph/camera/CameraObjectCallback.h>
#include <ves/xplorer/scenegraph/camera/CameraObject.h>

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/scenegraph/Group.h>
#include <ves/xplorer/scenegraph/CoordinateSystemTransform.h>

#include <osg/Camera>

// --- vrJuggler Includes --- //
#include <gmtl/Xforms.h>

using namespace ves::xplorer::scenegraph::camera;

////////////////////////////////////////////////////////////////////////////////
CameraObjectCallback::CameraObjectCallback()
:
osg::Object(),
osg::NodeCallback()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
CameraObjectCallback::CameraObjectCallback( const CameraObjectCallback& input )
:
osg::Object( input ),
osg::NodeCallback( input )
{
    if( &input != this )
    {
        ;
    }
}
////////////////////////////////////////////////////////////////////////////////
CameraObjectCallback::~CameraObjectCallback()
{
}
////////////////////////////////////////////////////////////////////////////////
void CameraObjectCallback::operator()( osg::Node* node, osg::NodeVisitor* nv )
{
    osg::ref_ptr< CameraObject > cameraEntity =
        static_cast< CameraObject* >( node );

    if( cameraEntity.valid() )
    {
        osg::ref_ptr< ves::xplorer::scenegraph::CoordinateSystemTransform >
            coordinateSystemTransform =
                new ves::xplorer::scenegraph::CoordinateSystemTransform(
                    &(cameraEntity->GetPluginDCS()),
                    &(cameraEntity->GetCameraDCS()) );

        gmtl::Matrix44d localToWorldMatrix =
            coordinateSystemTransform->GetTransformationMatrix();

        osg::Matrixd tempMatrix( localToWorldMatrix.getData() );
        tempMatrix = osg::Matrix::inverse( tempMatrix ) *
                     cameraEntity->GetInitialViewMatrix();
        
        cameraEntity->GetCamera().setViewMatrix( tempMatrix );
        cameraEntity->CalculateMatrixMVPT();
    }

    traverse( node, nv );
}
////////////////////////////////////////////////////////////////////////////////
