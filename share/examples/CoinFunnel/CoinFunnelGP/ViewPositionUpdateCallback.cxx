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
#include "ViewPositionUpdateCallback.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/DCS.h>

// --- vrJuggler Includes --- //
#include <gmtl/Xforms.h>
#include <gmtl/Generate.h>
#include <gmtl/Matrix.h>
#include <gmtl/Vec.h>

// --- OSG Includes --- //
#include <osg/NodeVisitor>
#include <osg/Vec3>

namespace demo
{

////////////////////////////////////////////////////////////////////////////////
ViewPositionUpdateCallback::ViewPositionUpdateCallback()
{
    m_head.init( "VJHead" );
}
////////////////////////////////////////////////////////////////////////////////
ViewPositionUpdateCallback::ViewPositionUpdateCallback( const ViewPositionUpdateCallback& input )
:
osg::Object( input ),
osg::Uniform::Callback( input )
{
    m_head.init( "VJHead" );
}
////////////////////////////////////////////////////////////////////////////////
void ViewPositionUpdateCallback::operator()( osg::Uniform* uniform, osg::NodeVisitor* nv )
{
    if( uniform )
    {
        gmtl::Matrix44d headMatrix = convertTo< double >( m_head->getData() );
        gmtl::Point3d headPosition = gmtl::makeTrans< gmtl::Point3d >( headMatrix );
        osg::Vec3d viewPosition;
        viewPosition.set( headPosition.mData[ 0 ], headPosition.mData[ 1 ], headPosition.mData[ 2 ] );
        uniform->set( viewPosition );
    }
}
////////////////////////////////////////////////////////////////////////////////

} // end demo
