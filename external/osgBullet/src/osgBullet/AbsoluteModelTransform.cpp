//
// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix  Software LLC.
// All rights reserved.
//

#include <osgBullet/AbsoluteModelTransform.h>
#include <osgUtil/CullVisitor>
#include <osg/NodeVisitor>
#include <osg/Transform>
#include <osg/Matrix>

#include <string>
#include <osg/io_utils>


namespace osgBullet
{


AbsoluteModelTransform::AbsoluteModelTransform()
{
    setReferenceFrame( osg::Transform::ABSOLUTE_RF );
}
AbsoluteModelTransform::AbsoluteModelTransform( const osg::Matrix& m )
  : _matrix( m )
{
    setReferenceFrame( osg::Transform::ABSOLUTE_RF );
}
AbsoluteModelTransform::AbsoluteModelTransform( const AbsoluteModelTransform& rhs, const osg::CopyOp& copyop )
  : osg::Transform( rhs, copyop ),
    _matrix( rhs._matrix )
{
    setReferenceFrame( osg::Transform::ABSOLUTE_RF );
}
AbsoluteModelTransform::~AbsoluteModelTransform()
{
}


bool
AbsoluteModelTransform::computeLocalToWorldMatrix( osg::Matrix& matrix, osg::NodeVisitor* nv ) const
{
    if( getReferenceFrame() == osg::Transform::ABSOLUTE_RF )
    {
        osg::Matrix view;
        if( !nv )
            osg::notify( osg::WARN ) << "AbsoluteModelTransform: NULL NodeVisitor; can't get view." << std::endl;
        else if( nv->getVisitorType() != osg::NodeVisitor::CULL_VISITOR )
            osg::notify( osg::WARN ) << "AbsoluteModelTransform: NodeVisitor is not CullVisitor; can't get view." << std::endl;
        else
        {
            osgUtil::CullVisitor* cv = dynamic_cast< osgUtil::CullVisitor* >( nv );
            osg::Camera* cam = cv->getCurrentCamera();
            cam->computeLocalToWorldMatrix( view, cv );
        }
        matrix = ( _matrix * view );
    }
    else
        // RELATIVE_RF
        matrix.preMult(_matrix);

    return( true );
}

bool
AbsoluteModelTransform::computeWorldToLocalMatrix( osg::Matrix& matrix, osg::NodeVisitor* nv ) const
{
    osg::Matrix inv( osg::Matrix::inverse( _matrix ) );
    if( getReferenceFrame() == osg::Transform::ABSOLUTE_RF )
    {
        osg::Matrix invView;
        if( !nv )
            osg::notify( osg::WARN ) << "AbsoluteModelTransform: NULL NodeVisitor; can't get invView." << std::endl;
        else if( nv->getVisitorType() != osg::NodeVisitor::CULL_VISITOR )
            osg::notify( osg::WARN ) << "AbsoluteModelTransform: NodeVisitor is not CullVisitor; can't get invView." << std::endl;
        else
        {
            osgUtil::CullVisitor* cv = dynamic_cast< osgUtil::CullVisitor* >( nv );
            osg::Camera* cam = cv->getCurrentCamera();
            cam->computeWorldToLocalMatrix( invView, cv );
        }
        matrix = ( invView * inv );
    }
    else
        // RELATIVE_RF
        matrix.postMult( inv );

    return( true );
}



}
