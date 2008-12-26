/*
 *
 * Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
 * All rights reserved.
 *
 */

#include <osgBullet/CollisionShapes.h>
#include <osgBullet/ComputeTriMeshVisitor.h>
#include <osgBullet/ComputeCylinderVisitor.h>
#include <osgBullet/Utils.h>

#include <osg/ComputeBoundsVisitor>
#include <osg/ShapeDrawable>
#include <osg/Geometry>
#include <osg/Geode>
#include <osg/MatrixTransform>

#include <osg/io_utils>
#include <iostream>


using namespace osgBullet;


btSphereShape* osgBullet::btSphereCollisionShapeFromOSG( osg::Node* node )
{
    osg::BoundingSphere sphere = node->getBound();
    float radius = sphere.radius();

    btSphereShape* shape = new btSphereShape( radius );

    return( shape );
}

btBoxShape* osgBullet::btBoxCollisionShapeFromOSG( osg::Node* node, const osg::BoundingBox* bb )
{
    osg::BoundingBox bbox;
    if (bb)
        bbox = *bb;
    else
    {
        osg::ComputeBoundsVisitor visitor;
        node->accept( visitor );
        bbox = visitor.getBoundingBox();
    }

    btBoxShape* shape = new btBoxShape( btVector3( ( bbox.xMax() - bbox.xMin() ) / 2.0,
                                                   ( bbox.yMax() - bbox.yMin() ) / 2.0,
                                                   ( bbox.zMax() - bbox.zMin() ) / 2.0 ) );
    return( shape );
}

btCylinderShape* osgBullet::btCylinderCollisionShapeFromOSG( osg::Node* node,
                                                        AXIS axis )
{
    ComputeCylinderVisitor visitor;

    switch( axis )
    {
        case _X:
        case X:
            visitor.setAxis( osg::X_AXIS );
            break;

        case _Y:
        case Y:
            visitor.setAxis( osg::Y_AXIS );
            break;

        case _Z:
        case Z:
            visitor.setAxis( osg::Z_AXIS );
            break;
    }
    node->accept( visitor );

    BoundingCylinder cyl = visitor.getBoundingCylinder();
    if( cyl.getRadius() <= 0. )
    {
        osg::notify( osg::FATAL ) << "NULL bounding cylinder." << std::endl;
        return( NULL );
    }

    btCylinderShape* shape;
    switch( axis )
    {
        case _X:
        case X:
            shape = new btCylinderShapeX( btVector3( cyl.getLength(), cyl.getRadius(), 0 ) );
            break;

        case _Y:
        case Y:
            shape = new btCylinderShape( btVector3( cyl.getRadius(), cyl.getLength(), 0 ) );
            break;

        case _Z:
        case Z:
            shape = new btCylinderShapeZ( btVector3( cyl.getRadius(), 0, cyl.getLength() ) );
    }
    return( shape );
}

btTriangleMeshShape* osgBullet::btTriMeshCollisionShapeFromOSG( osg::Node* node )
{
    ComputeTriMeshVisitor visitor;
    node->accept( visitor );

    osg::Vec3Array* vertices = visitor.getTriMesh();
    if( vertices->size() < 3 )
    {
        osg::notify( osg::WARN ) << "osgBullet::btTriMeshCollisionShapeFromOSG, no triangles found" << std::endl;
        return( NULL );
    }

    btTriangleMesh* mesh = new btTriangleMesh( true, false );
    for( size_t i = 0; i + 3 < vertices->size(); i += 3 )
    {
        osg::Vec3& p1 = ( *vertices )[ i ];
        osg::Vec3& p2 = ( *vertices )[ i + 1 ];
        osg::Vec3& p3 = ( *vertices )[ i + 2 ];
        mesh->addTriangle( btVector3( p1.x(), p1.y(), p1.z() ),
                          btVector3( p2.x(), p2.y(), p2.z() ),
                          btVector3( p3.x(), p3.y(), p3.z() ) );
    }

    btBvhTriangleMeshShape* meshShape = new btBvhTriangleMeshShape( mesh, true );
    return( meshShape );
}

btConvexTriangleMeshShape* osgBullet::btConvexTriMeshCollisionShapeFromOSG( osg::Node* node )
{
    ComputeTriMeshVisitor visitor;
    node->accept( visitor );

    osg::Vec3Array* vertices = visitor.getTriMesh();

    btTriangleMesh* mesh = new btTriangleMesh;
    osg::Vec3 p1, p2, p3;
    for( size_t i = 0; i < vertices->size(); i += 3 )
    {
        p1 = vertices->at( i );
        p2 = vertices->at( i + 1 );
        p3 = vertices->at( i + 2 );
        mesh->addTriangle( btVector3( p1.x(), p1.y(), p1.z() ),
                          btVector3( p2.x(), p2.y(), p2.z() ),
                          btVector3( p3.x(), p3.y(), p3.z() ) );
    }

    btConvexTriangleMeshShape* meshShape = new btConvexTriangleMeshShape( mesh );
    return( meshShape );
}

osg::Node* osgBullet::osgNodeFromBtCollisionShape( const btCollisionShape* btShape, const btTransform& trans )
{
    if( btShape->getShapeType() == BOX_SHAPE_PROXYTYPE )
    {
        const btBoxShape* btBox = dynamic_cast< const btBoxShape* >( btShape );
        return( osgNodeFromBtCollisionShape( btBox, trans ) );
    }
    else if( btShape->getShapeType() == SPHERE_SHAPE_PROXYTYPE )
    {
        const btSphereShape* btSphere = dynamic_cast< const btSphereShape* >( btShape );
        return( osgNodeFromBtCollisionShape( btSphere, trans ) );
    }
    else if( btShape->getShapeType() == CYLINDER_SHAPE_PROXYTYPE )
    {
        const btCylinderShape* btCylinder = dynamic_cast< const btCylinderShape* >( btShape );
        return( osgNodeFromBtCollisionShape( btCylinder, trans ) );
    }
    else if( btShape->getShapeType() == TRIANGLE_MESH_SHAPE_PROXYTYPE )
    {
        const btBvhTriangleMeshShape* btTriMesh = dynamic_cast< const btBvhTriangleMeshShape* >( btShape );
        // Do NOT pass in a transform. Unlike cylinder, sphere, and box,
        // tri meshes are always in absolute space.
        return( osgNodeFromBtCollisionShape( btTriMesh ) );
    }
    else if( btShape->getShapeType() == CONVEX_TRIANGLEMESH_SHAPE_PROXYTYPE )
    {
        const btConvexTriangleMeshShape* btConvexTriMesh = dynamic_cast< const btConvexTriangleMeshShape* >( btShape );
        // Do NOT pass in a transform. Unlike cylinder, sphere, and box,
        // tri meshes are always in absolute space.
        return( osgNodeFromBtCollisionShape( btConvexTriMesh ) );
    }
    else if( btShape->getShapeType() == COMPOUND_SHAPE_PROXYTYPE )
    {
        const btCompoundShape* masterShape = dynamic_cast< const btCompoundShape* >( btShape );
        osg::Group* grp = new osg::Group;
        int idx;
        for (idx=0; idx< masterShape->getNumChildShapes(); idx++)
        {
            const btCollisionShape* s = masterShape->getChildShape( idx );
            const btTransform t = masterShape->getChildTransform( idx );
            const btTransform accumTrans = trans * t;
            grp->addChild( osgNodeFromBtCollisionShape( s, accumTrans ) );
        }
        return( grp );
    }
    else
    {
        osg::notify( osg::FATAL ) << "osgNodeFromBtCollisionShape: Unsupported shape type: " <<
        btShape->getShapeType() << std::endl;
        return( NULL );
    }
}

osg::Node* osgBullet::osgNodeFromBtCollisionShape( const btBoxShape* btBox, const btTransform& trans )
{
    btVector3 halfs = btBox->getHalfExtentsWithMargin();

    osg::Box* box = new osg::Box();
    box->setHalfLengths( osg::Vec3( halfs.x(), halfs.y(), halfs.z() ) );

    osg::ShapeDrawable* shape = new osg::ShapeDrawable( box );
    shape->setColor( osg::Vec4( 1., 1., 1., 1. ) );
    osg::Geode* geode = new osg::Geode();
    geode->addDrawable( shape );

    osg::Matrix m = asOsgMatrix( trans );
    if (m.isIdentity())
        return( geode );
    else
    {
        osg::MatrixTransform* mt = new osg::MatrixTransform;
        mt->setMatrix( m );
        mt->addChild( geode );
        return mt;
    }
}

osg::Node * osgBullet::osgNodeFromBtCollisionShape( const btSphereShape * btSphere, const btTransform& trans )
{
    osg::Sphere* sphere = new osg::Sphere();
    sphere->setRadius( btSphere->getRadius() );

    osg::TessellationHints* hints = new osg::TessellationHints();
    hints->setDetailRatio( .2 );

    osg::ShapeDrawable* shape = new osg::ShapeDrawable( sphere, hints );
    shape->setColor( osg::Vec4( 1., 1., 1., 1. ) );
    osg::Geode* geode = new osg::Geode();
    geode->addDrawable( shape );

    osg::Matrix m = asOsgMatrix( trans );
    if (m.isIdentity())
        return( geode );
    else
    {
        osg::MatrixTransform* mt = new osg::MatrixTransform;
        mt->setMatrix( m );
        mt->addChild( geode );
        return mt;
    }
}

osg::Node * osgBullet::osgNodeFromBtCollisionShape( const btCylinderShape * btCylinder, const btTransform& trans )
{
    osg::Cylinder* cylinder = new osg::Cylinder();
    cylinder->setRadius( btCylinder->getRadius() );

    switch( btCylinder->getUpAxis() )
    {
        case _X:
        case X:
            cylinder->setHeight( 2 * btCylinder->getHalfExtentsWithMargin().getX() );
            cylinder->setRotation( osg::Quat( osg::PI_2, osg::Vec3( 0, 1, 0 ) ) );
            break;

        case _Y:
        case Y:
            cylinder->setHeight( 2 * btCylinder->getHalfExtentsWithMargin().getY() );
            cylinder->setRotation( osg::Quat( osg::PI_2, osg::Vec3( 1, 0, 0 ) ) );
            break;

        case _Z:
        case Z:
            cylinder->setHeight( 2 * btCylinder->getHalfExtentsWithMargin().getZ() );
    }

    osg::TessellationHints* hints = new osg::TessellationHints();
    hints->setDetailRatio( .2 );

    osg::ShapeDrawable* shape = new osg::ShapeDrawable( cylinder, hints );
    shape->setColor( osg::Vec4( 1., 1., 1., 1. ) );
    osg::Geode* geode = new osg::Geode();
    geode->addDrawable( shape );

    osg::Matrix m = asOsgMatrix( trans );
    if (m.isIdentity())
        return( geode );
    else
    {
        osg::MatrixTransform* mt = new osg::MatrixTransform;
        mt->setMatrix( m );
        mt->addChild( geode );
        return mt;
    }
}

osg::Node* osgBullet::osgNodeFromBtCollisionShape( const btTriangleMeshShape* btTriMesh, const btTransform& trans )
{
    const btTriangleMesh* mesh = dynamic_cast< const btTriangleMesh* >( btTriMesh->getMeshInterface() );
    if( !mesh )
    {
        osg::notify( osg::FATAL ) << "osgNodeFromBtCollisionShape: No triangle mesh." << std::endl;
        return( NULL );
    }

    btVector3* verts;
    int* indices;
    int numVerts;
    int numFaces;
    PHY_ScalarType vt, ft;
    int vs, fs;

    mesh->getLockedReadOnlyVertexIndexBase( ( const unsigned char** )&verts, numVerts, vt, vs, ( const unsigned char** )&indices, fs, numFaces, ft );

    osg::Vec3Array* vec = new osg::Vec3Array();
    vec->resize( numVerts );
    int idx;
    for( idx = 0; idx < numVerts; idx++ )
    {
        const btVector3& bulletVert = verts[ idx ];
        ( *vec )[ idx ].set( bulletVert.getX(), bulletVert.getY(), bulletVert.getZ() );
    }

    osg::IntArray* index = new osg::IntArray();
    index->resize( numFaces * 3 );
    for( idx = 0; idx < numFaces * 3; idx++ )
    {
        ( *index )[ idx ] = indices[ idx ];
    }

    osg::Vec4Array* color = new osg::Vec4Array();
    color->push_back( osg::Vec4( 1., 1., 1., 1. ) );

    osg::Geometry* geom = new osg::Geometry;
    geom->setVertexArray( vec );
    geom->setVertexIndices( index );
    geom->setColorArray( color );
    geom->setColorBinding( osg::Geometry::BIND_OVERALL );

    geom->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::TRIANGLES, 0, numVerts ) );

    osg::Geode* geode = new osg::Geode();
    geode->addDrawable( geom );

    osg::Matrix m = asOsgMatrix( trans );
    if (m.isIdentity())
        return( geode );
    else
    {
        osg::MatrixTransform* mt = new osg::MatrixTransform;
        mt->setMatrix( m );
        mt->addChild( geode );
        return mt;
    }
}

osg::Node* osgBullet::osgNodeFromBtCollisionShape( const btConvexTriangleMeshShape* btTriMesh, const btTransform& trans )
{
    const btTriangleMesh* mesh = dynamic_cast< const btTriangleMesh * >( btTriMesh->getMeshInterface() );
    if( !mesh )
    {
        osg::notify( osg::FATAL ) << "osgNodeFromBtCollisionShape: No triangle mesh." << std::endl;
        return( NULL );
    }

    btVector3* verts;
    int* indices;
    int numVerts;
    int numFaces;
    PHY_ScalarType vt, ft;
    int vs, fs;

    mesh->getLockedReadOnlyVertexIndexBase( ( const unsigned char** )&verts, numVerts, vt, vs, ( const unsigned char** )&indices, fs, numFaces, ft );

    osg::Vec3Array* vec = new osg::Vec3Array();
    vec->resize( numVerts );
    int idx;
    for( idx = 0; idx < numVerts; idx++ )
    {
        const btVector3& bulletVert = verts[ idx ];
        ( *vec )[ idx ].set( bulletVert.getX(), bulletVert.getY(), bulletVert.getZ() );
    }

    osg::IntArray* index = new osg::IntArray();
    index->resize( numFaces * 3 );
    for( idx = 0; idx < numFaces * 3; idx++ )
    {
        ( *index )[ idx ] = indices[ idx ];
    }

    osg::Vec4Array* color = new osg::Vec4Array();
    color->push_back( osg::Vec4( 1., 1., 1., 1. ) );

    osg::Geometry* geom = new osg::Geometry;
    geom->setVertexArray( vec );
    geom->setVertexIndices( index );
    geom->setColorArray( color );
    geom->setColorBinding( osg::Geometry::BIND_OVERALL );

    geom->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::TRIANGLES, 0, numVerts ) );

    osg::Geode* geode = new osg::Geode();
    geode->addDrawable( geom );

    osg::Matrix m = asOsgMatrix( trans );
    if (m.isIdentity())
        return( geode );
    else
    {
        osg::MatrixTransform* mt = new osg::MatrixTransform;
        mt->setMatrix( m );
        mt->addChild( geode );
        return mt;
    }
}

