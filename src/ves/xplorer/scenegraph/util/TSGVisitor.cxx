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

// --- C/C++ Includes --- //
#include <iostream>

using namespace ves::xplorer::scenegraph::util;
namespace vxsu = ves::xplorer::scenegraph::util;

////////////////////////////////////////////////////////////////////////////////
TSGVisitor::TSGVisitor()
    :
    osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN )
{
    ;
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
        osg::Geometry* geo =
            dynamic_cast< osg::Geometry* >( geode.getDrawable( i ) );
        if( geo )
        {
            //_bm->prepareGeometry( geo );
        }
    }

    osg::NodeVisitor::apply( geode );
}
////////////////////////////////////////////////////////////////////////////////
void TSGVisitor::GenerateTangentSpaceData(
    unsigned int tangentIndex,
    unsigned int binormalIndex,
    unsigned int normalIndex )
{
    /*
    //Create the tangent, binormal, and normal array data
    osg::ref_ptr< osgUtil::TangentSpaceGenerator > tsg =
        new osgUtil::TangentSpaceGenerator;
    tsg->generate( geo, _normal_unit );

    //Assign the tangent array data
    if( !geo->getVertexAttribArray( tangentIndex ) )
    {
        geo->setVertexAttribData(
            tangentIndex,
            osg::Geometry::ArrayData(
                tsg->getTangentArray(),
                osg::Geometry::BIND_PER_VERTEX, GL_FALSE ) );
    }
    else
    {

    }

    //Assign the binormal array data
    if( !geo->getVertexAttribArray( binormalIndex ) )
    {
        geo->setVertexAttribData(
            binormalIndex,
            osg::Geometry::ArrayData(
                tsg->getBinormalArray(),
                osg::Geometry::BIND_PER_VERTEX, GL_FALSE ) );
    }
    else
    {

    }

    //Assign the normal array data
    if( !geo->getVertexAttribArray( normalIndex ) )
    {
        geo->setVertexAttribData(
            normalIndex,
            osg::Geometry::ArrayData(
                tsg->getNormalArray(),
                osg::Geometry::BIND_PER_VERTEX, GL_FALSE ) );
    }
    else
    {

    }
    */
}
////////////////////////////////////////////////////////////////////////////////
