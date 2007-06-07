/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * Date modified: $Date: 2007-04-10 19:18:51 -0500 (Tue, 10 Apr 2007) $
 * Version:       $Rev: 7298 $
 * Author:        $Author: jbkoch $
 * Id:            $Id: LocalToWorldTransform.cxx 7298 2007-04-11 00:18:51Z jbkoch $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
// --- VE-Suite Includes --- //
#include "VE_Xplorer/SceneGraph/Utilities/LocalToWorldTransform.h"

// --- OSG Includes --- //
#include <osg/PositionAttitudeTransform>

#include <gmtl/Xforms.h>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace VE_SceneGraph::Utilities;

////////////////////////////////////////////////////////////////////////////////
LocalToWorldTransform::LocalToWorldTransform( osg::PositionAttitudeTransform* worldNode, osg::PositionAttitudeTransform* localNode )
:
NodeVisitor( TRAVERSE_PARENTS )
{
    gmtl::identity( m_localToWorldTransform );

    m_worldNode = worldNode;
    m_localNode = localNode;
    localNode->accept( *this );
}
////////////////////////////////////////////////////////////////////////////////
LocalToWorldTransform::~LocalToWorldTransform()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////    
void LocalToWorldTransform::apply( osg::PositionAttitudeTransform& pat )
{
    if( pat.getName() == m_worldNode->getName() )
    {
        //m_localToWorldTransform.set( osg::computeLocalToWorld( _nodePath ).ptr() );

        for( size_t i = 0; i < _nodePath.size(); ++i )
        {
            osg::Matrixd matrix;
            matrix.identity();
            osg::ref_ptr< osg::Node > node = _nodePath.at( i );
            matrix.setTrans( static_cast< osg::PositionAttitudeTransform* >( node.get() )->getPosition() );
            matrix.setRotate( static_cast< osg::PositionAttitudeTransform* >( node.get() )->getAttitude() );

            gmtl::Matrix44d temp;
            gmtl::identity( temp );
            temp.set( matrix.ptr() );

            m_localToWorldTransform *= temp;
        }
        
        //Premultiply by the localNode transform since it is not in the node path
        osg::Matrix localTransform;
        localTransform.identity();
        localTransform.setTrans( m_localNode->getPosition() );
        localTransform.setRotate( m_localNode->getAttitude() );

        gmtl::Matrix44d local;
        local.set( localTransform.ptr() );

        m_localToWorldTransform = m_localToWorldTransform * gmtl::invert( local );

        return;
    }

    osg::NodeVisitor::apply( pat );
}
////////////////////////////////////////////////////////////////////////////////
gmtl::Matrix44d& LocalToWorldTransform::GetLocalToWorldTransform()
{
    return m_localToWorldTransform;
}
////////////////////////////////////////////////////////////////////////////////
