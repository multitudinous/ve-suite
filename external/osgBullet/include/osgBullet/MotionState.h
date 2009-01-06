// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC. All rights reserved.

#ifndef __OSGBULLET_MOTIONSTATE_H__
#define __OSGBULLET_MOTIONSTATE_H__

#include <osg/MatrixTransform>
#include <osgBullet/AbsoluteModelTransform.h>


#include <btBulletCollisionCommon.h>

#include <osgBullet/Export.h>


namespace osgBullet {



/*!
    A btMotionState that allows Bullet to set the ransformation
    of an OSG subgraph corresponding to a rigid body.
    
    This class can interface with an osg::MatrixTransform
    or an osgBullet::AbsoluteModelTransform.

    Typical usage:
     - Call setTransform() to attach the root node of a subgraph.
       The node must be a MatrixTransform or AbsoluteModelTransform.
     - Call setParentTransform() to specify the initial transformation
       for the subgraph.
     - Call setCenterOfMass() to specify the xyz point corresponding
       to the origin of the Bullet collision shape used by the rigid body.
*/

class OSGBULLET_EXPORT MotionState : public btMotionState
{
public:
    MotionState( const osg::Matrix& parentTransform = osg::Matrix::identity(),
        const osg::Vec3& centerOfMass = osg::Vec3( 0., 0., 0. ) );
    ~MotionState( void ) { }


    // Bullet interface routines. To be used by Bullet only.
    // Note that setWorldTransform is called by resetTransform.
    virtual void	setWorldTransform(const btTransform& worldTrans);
    virtual void	getWorldTransform(btTransform& worldTrans ) const;


    // Attach a subgraph that corresponds to the rigid body owning
    // this MotionState. MotionState removes the center of mass
    // offset from its transformation before driving this subgraph.
    void setTransform( osg::Transform* transform );
    osg::Transform* getTransform();
    const osg::Transform* getTransform() const;

    // Attach an optional transform for debugging purposes.
    // Bullet will drive this subgraph with the same matrix used
    // to drive the rigid body.
    void setDebugTransform( osg::Transform* transform );
    osg::Transform* getDebugTransform();
    const osg::Transform * getDebugTransform() const;

    // Set the initial transformation for the subgraph. Typically,
    // this is the accumulated model transformations of ancestor nodes.
    void setParentTransform( const osg::Matrix m );
    osg::Matrix getParentTransform() const;

    // Set the center of mass. This is an xyz point in the subgraph's
    // local coordinates that corresponds to the origin in the
    // collision shape's local coordinates.
    void setCenterOfMass( const osg::Vec3& com );
    osg::Vec3 getCenterOfMass() const;

    // This is a convenience routine that calls setWorldTransform with
    // the concatenation of the center of mass and parent transform.
    // It is called by setCenterOfMass and setParentTransform to set the
    // initial world transformation. See setWorldTransformation for more
    // details.
    // Apps can call this method directly to invoke setWorldTransform() with
    // the concatentation of the center of mass and parent transforms.
    void resetTransform();

private:
    // One or the other of these will be valid, depending on whether the
    // MotionState is associated with an AbsoluteModelTransform or a
    // plain old MatrixTransform.
    osg::ref_ptr< osg::MatrixTransform > _mt;
    osg::ref_ptr< osgBullet::AbsoluteModelTransform > _amt;

    osg::ref_ptr< osg::MatrixTransform > _debugMT;
    osg::ref_ptr< osgBullet::AbsoluteModelTransform > _debugAMT;

    // This is the accumulated model-to-world matrix of parent Transform nodes
    // in the scene graph.
    osg::Matrix _parentTransform;
    // _com is used to align the origin-centered collision shape with
    // an off-origin OSG visual representation.
    osg::Vec3 _com;

    // This is the transformation of the collision shape / rigid body within the Bullet physics simulation.
    // See setWorldTransformation for more details.
    btTransform _transform;
};


} // namespace osgBullet

#endif // __OSGBULLET_MOTIONSTATE_H__
