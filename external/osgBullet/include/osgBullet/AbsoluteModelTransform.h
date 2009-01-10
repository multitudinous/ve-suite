// Copyright 2008 Blue Newt Software LLC and Skew Matrix  Software LLC. All rights reserved.

#ifndef __OSGBULLET_ABSOLUTE_MODEL_TRANSFORM_H__
#define __OSGBULLET_ABSOLUTE_MODEL_TRANSFORM_H__


#include <osg/NodeVisitor>
#include <osg/Transform>
#include <osgBullet/Export.h>

#include <string>


namespace osgBullet {


/*!
   AbsoluteModelTransform -- An OSG Transform that overloads ABSOLUTE_RF to
   preserve the view. Regardless of (non-Camera) transforms above this node in
   the hierarchy, the effective transform will be this transform concatenated
   with the view. This allows Bullet to drive a model's transform with a single
   matrix.

   This behavior can be disabled by setting the reference frame to RELATIVE_RF,
   which causes this Transform to behave like a regular MatrixTransform.

   The default reference frame is ABSOLUTE_RF.
*/
class OSGBULLET_EXPORT AbsoluteModelTransform : public osg::Transform
{
public:
    AbsoluteModelTransform();
    AbsoluteModelTransform( const osg::Matrix& m );
    AbsoluteModelTransform( const AbsoluteModelTransform& rhs, const osg::CopyOp& copyop=osg::CopyOp::SHALLOW_COPY );

    META_Node( osgBullet, AbsoluteModelTransform );

    virtual bool computeLocalToWorldMatrix( osg::Matrix& matrix, osg::NodeVisitor* nv ) const;
    virtual bool computeWorldToLocalMatrix( osg::Matrix& matrix, osg::NodeVisitor* nv ) const;

    inline void setMatrix( const osg::Matrix& m ) { _matrix = m; dirtyBound(); }
    inline const osg::Matrix& getMatrix() const { return _matrix; }


protected:
    virtual ~AbsoluteModelTransform();

    osg::Matrix _matrix;
};

} // end namespace osgBullet

#endif // __OSGBULLET_ABSOLUTE_MODEL_TRANSFORM_H__
