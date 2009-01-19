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
#include <ves/xplorer/scenegraph/physics/osgToBullet.h>

// --- OSG Stuff --- //
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/TriangleIndexFunctor>

// --- Bullet Includes --- //
#include <BulletCollision/CollisionShapes/btTriangleMesh.h>

// --- C/C++ Libraries --- //
#include <iostream>

class TriIndexFunc
{
public:
    TriIndexFunc()
    {
        ;
    }
    ~TriIndexFunc()
    {
        ;
    }

    void inline operator()( unsigned int pos1, unsigned int pos2, unsigned int pos3 )
    {
        m_triangleIndex.push_back( pos1 );
        m_triangleIndex.push_back( pos2 );
        m_triangleIndex.push_back( pos3 );
    }

    std::vector< unsigned int > m_triangleIndex;
};

namespace ves
{
namespace xplorer
{
namespace scenegraph
{

////////////////////////////////////////////////////////////////////////////////
osgToBullet::osgToBullet( osg::Node* node )
        :
        m_triangleMesh( new btTriangleMesh() ),
        NodeVisitor( TRAVERSE_ALL_CHILDREN )
{
    node->accept( *this );
}
////////////////////////////////////////////////////////////////////////////////
osgToBullet::~osgToBullet()
{
    delete m_triangleMesh;
}

////////////////////////////////////////////////////////////////////////////////
void osgToBullet::apply( osg::Geode& geode )
{
    for( size_t i = 0; i < geode.getNumDrawables(); ++i )
    {
        osg::TriangleIndexFunctor< TriIndexFunc > tif;
        osg::ref_ptr< osg::Drawable > drawable = geode.getDrawable( i );
        drawable->accept( tif );
        m_boundingBox.expandBy( drawable->getBound() );
        m_boundingSphere.expandBy( drawable->getBound() );

        osg::ref_ptr< osg::Vec3Array > vertexArray;
        vertexArray = static_cast< osg::Vec3Array* >( drawable->asGeometry()->getVertexArray() );

        for( size_t j = 0; j < tif.m_triangleIndex.size() / 3; ++j )
        {
            unsigned int index1, index2, index3;
            index1 = tif.m_triangleIndex.at( j * 3 );
            index2 = tif.m_triangleIndex.at( j * 3 + 1 );
            index3 = tif.m_triangleIndex.at( j * 3 + 2 );

            osg::Vec3d point1, point2, point3;
            point1 = vertexArray->at( index1 );
            point2 = vertexArray->at( index2 );
            point3 = vertexArray->at( index3 );

            m_triangleMesh->addTriangle( btVector3( point1.x(), point1.y(), point1.z() ),
                                         btVector3( point2.x(), point2.y(), point2.z() ),
                                         btVector3( point3.x(), point3.y(), point3.z() ) );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
btTriangleMesh* osgToBullet::GetTriangleMesh()
{
    return m_triangleMesh;
}
////////////////////////////////////////////////////////////////////////////////
osg::BoundingBox osgToBullet::GetBoundingBox()
{
    return m_boundingBox;
}
////////////////////////////////////////////////////////////////////////////////
osg::BoundingSphere osgToBullet::GetBoundingSphere()
{
    return m_boundingSphere;
}
////////////////////////////////////////////////////////////////////////////////

} // end scenegraph
} // end xplorer
} // end ves
