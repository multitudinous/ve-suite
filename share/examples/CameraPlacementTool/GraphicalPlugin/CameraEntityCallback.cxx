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

// --- My Includes --- //
#include "CameraEntityCallback.h"
#include "CameraEntity.h"
#include "CustomKeyboardMouseInterface.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/scenegraph/Group.h>
#include <ves/xplorer/scenegraph/LocalToWorldTransform.h>

// --- vrJuggler Includes --- //
#include <gmtl/Xforms.h>

using namespace cpt;

////////////////////////////////////////////////////////////////////////////////
CameraEntityCallback::CameraEntityCallback()
:
osg::Object(),
osg::NodeCallback(),
mCustomKeyboardMouseInterface( new cpt::CustomKeyboardMouseInterface() )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
CameraEntityCallback::CameraEntityCallback( const CameraEntityCallback& input )
:
osg::Object( input ),
osg::NodeCallback( input ),
mCustomKeyboardMouseInterface( 0 )
{
    if( &input != this )
    {
        ;
    }
}
////////////////////////////////////////////////////////////////////////////////
CameraEntityCallback::~CameraEntityCallback()
{
    delete mCustomKeyboardMouseInterface;
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntityCallback::operator()( osg::Node* node, osg::NodeVisitor* nv )
{
    osg::ref_ptr< cpt::CameraEntity > cameraEntity =
        static_cast< cpt::CameraEntity* >( node );

    if( cameraEntity.valid() )
    {
        osg::ref_ptr< ves::xplorer::scenegraph::LocalToWorldTransform >
            localToWorldTransform =
                new ves::xplorer::scenegraph::LocalToWorldTransform(
                    cameraEntity->GetPluginDCS(), cameraEntity->GetDCS() );

        gmtl::Matrix44d localToWorldMatrix =
            localToWorldTransform->GetLocalToWorldTransform();

        osg::Matrixd tempMatrix( localToWorldMatrix.getData() );
        tempMatrix = osg::Matrix::inverse( tempMatrix ) *
                     cameraEntity->GetInitialViewMatrix();
        
        cameraEntity->setViewMatrix( tempMatrix );
        cameraEntity->CalculateMatrixMVPT();

        if( mCustomKeyboardMouseInterface->ProcessEvents() )
        {
            cameraEntity->CustomKeyboardMouseSelection(
                mCustomKeyboardMouseInterface->GetMousePosition(),
                localToWorldMatrix );
        }
    }

    traverse( node, nv );
}
////////////////////////////////////////////////////////////////////////////////
