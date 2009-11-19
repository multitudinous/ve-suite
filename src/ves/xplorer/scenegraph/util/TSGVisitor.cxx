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
#include <ves/xplorer/scenegraph/util/TSGVisitor.h>

// --- OSG Includes --- //
#include <osg/Geode>

#include <osgUtil/TangentSpaceGenerator>

// --- STL Includes --- //
#include <iostream>

using namespace ves::xplorer::scenegraph::util;

////////////////////////////////////////////////////////////////////////////////
TSGVisitor::TSGVisitor(
    osg::Node* const node,
    unsigned int normalMapTexUnit,
    unsigned int tangentIndex,
    unsigned int binormalIndex,
    unsigned int normalIndex )
    :
    osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN ),
    m_normalMapTexUnit( normalMapTexUnit ),
    m_tangentIndex( tangentIndex ),
    m_binormalIndex( binormalIndex ),
    m_normalIndex( normalIndex )
{
    node->accept( *this );
}
////////////////////////////////////////////////////////////////////////////////
TSGVisitor::~TSGVisitor()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void TSGVisitor::apply( osg::Geode& geode )
{
    for( unsigned int i = 0; i < geode.getNumDrawables(); ++i )
    {
        osg::Geometry* geometry =
            dynamic_cast< osg::Geometry* >( geode.getDrawable( i ) );
        if( geometry )
        {
            PrepareGeometry( geometry );
        }
    }

    osg::NodeVisitor::apply( geode );
}
////////////////////////////////////////////////////////////////////////////////
void TSGVisitor::PrepareGeometry( osg::Geometry* const geometry )
{
    //Create the tangent, binormal, and normal array data
    osg::ref_ptr< osgUtil::TangentSpaceGenerator > tsg =
        new osgUtil::TangentSpaceGenerator;
    tsg->generate( geometry, m_normalMapTexUnit );

    //Assign the tangent array data
    if( !geometry->getVertexAttribArray( m_tangentIndex ) )
    {
        geometry->setVertexAttribData(
            m_tangentIndex,
            osg::Geometry::ArrayData(
                tsg->getTangentArray(),
                osg::Geometry::BIND_PER_VERTEX, GL_FALSE ) );
    }
    else
    {
        ;
    }

    //Assign the binormal array data
    if( !geometry->getVertexAttribArray( m_binormalIndex ) )
    {
        geometry->setVertexAttribData(
            m_binormalIndex,
            osg::Geometry::ArrayData(
                tsg->getBinormalArray(),
                osg::Geometry::BIND_PER_VERTEX, GL_FALSE ) );
    }
    else
    {
        ;
    }

    //Assign the normal array data
    if( !geometry->getVertexAttribArray( m_normalIndex ) )
    {
        geometry->setVertexAttribData(
            m_normalIndex,
            osg::Geometry::ArrayData(
                tsg->getNormalArray(),
                osg::Geometry::BIND_PER_VERTEX, GL_FALSE ) );
    }
    else
    {
        ;
    }
}
////////////////////////////////////////////////////////////////////////////////
