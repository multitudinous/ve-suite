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
#include <ves/xplorer/scenegraph/CoordinateSystemTransform.h>
#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/scenegraph/SceneManager.h>

// --- VR Juggler Includes --- //
#include <gmtl/Xforms.h>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
CoordinateSystemTransform::CoordinateSystemTransform(
    osg::Node* stopNode,
    osg::Node* startNode,
    bool includeCameraTransform )
    :
    NodeVisitor( TRAVERSE_PARENTS ),
    mStopNode( stopNode ),
    mIncludeCameraTransform( includeCameraTransform )
{
    gmtl::identity( mStartToStopMatrix );
    gmtl::identity( mStartToStopMatrixWithoutLocal );

    startNode->accept( *this );
}
////////////////////////////////////////////////////////////////////////////////
CoordinateSystemTransform::~CoordinateSystemTransform()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CoordinateSystemTransform::apply( osg::Node& node )
{
    if( &node == mStopNode.get() )
    {
        ves::xplorer::scenegraph::DCS* localDCS = 
            dynamic_cast< ves::xplorer::scenegraph::DCS* >(
                _nodePath.at( _nodePath.size() - 1 ) );
        if( localDCS )
        {
            mStartToStopMatrix *= localDCS->GetMat();
        }

        if( mIncludeCameraTransform )
        {
            mStartToStopMatrixWithoutLocal *=
                ves::xplorer::scenegraph::SceneManager::instance()->
                    GetWorldDCS()->GetMat();
        }

        for( size_t i = 0; i < _nodePath.size() - 1; ++i )
        {
            ves::xplorer::scenegraph::DCS* dcs =
                dynamic_cast< ves::xplorer::scenegraph::DCS* >(
                    _nodePath.at( i ) );
            if( dcs )
            {
                mStartToStopMatrixWithoutLocal *= dcs->GetMat();
            }
        }

        mStartToStopMatrix =
            mStartToStopMatrixWithoutLocal * mStartToStopMatrix;

        return;
    }

    osg::NodeVisitor::apply( node );
}
////////////////////////////////////////////////////////////////////////////////
const gmtl::Matrix44d& CoordinateSystemTransform::GetTransformationMatrix(
    bool includeLocalTransform ) const
{
    if( includeLocalTransform )
    {
        return mStartToStopMatrix;
    }

    return mStartToStopMatrixWithoutLocal;
}
////////////////////////////////////////////////////////////////////////////////
