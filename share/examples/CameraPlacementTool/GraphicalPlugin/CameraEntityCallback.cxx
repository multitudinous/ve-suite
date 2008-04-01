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

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/DCS.h>

// --- vrJuggler Includes --- //
#include <gmtl/Xforms.h>

// --- OSG Includes --- //
#include <osg/TexGenNode>

using namespace cpt;

////////////////////////////////////////////////////////////////////////////////
CameraEntityCallback::CameraEntityCallback()
:
osg::Object(),
osg::NodeCallback(),
mInitialViewMatrix( osg::Matrix::identity() )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
CameraEntityCallback::CameraEntityCallback( const CameraEntityCallback& input )
:
osg::Object( input ),
osg::NodeCallback( input )
{
    if( &input != this )
    {
        mInitialViewMatrix = input.mInitialViewMatrix;
    }
}
////////////////////////////////////////////////////////////////////////////////
CameraEntityCallback::~CameraEntityCallback()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntityCallback::operator()( osg::Node* node, osg::NodeVisitor* nv )
{
    osg::ref_ptr< cpt::CameraEntity > cameraEntity =
        static_cast< cpt::CameraEntity* >( node );

    if( cameraEntity.valid() )
    {
        gmtl::Matrix44d cameraDCSMatrix = cameraEntity->GetDCS()->GetMat();
        osg::Matrixd inverseCameraDCSMatrix(
            gmtl::invert( cameraDCSMatrix ).getData() );

        osg::Matrixd currentViewMatrix =
            inverseCameraDCSMatrix * mInitialViewMatrix;
        
        cameraEntity->setViewMatrix( currentViewMatrix );
        
        cameraEntity->CalculateMatrixMVPT();
        cameraEntity->GetTexGenNode()->getTexGen()->setPlanesFromMatrix(
            cameraEntity->GetMatrixMVPT() );
    }

    traverse( node, nv );
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntityCallback::SetInitialViewMatrix(
    const osg::Matrixd& initialViewMatrix )
{
    mInitialViewMatrix = initialViewMatrix;
}
////////////////////////////////////////////////////////////////////////////////