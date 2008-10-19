// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC. All rights reserved.

#ifndef __OSGBULLET_MOTIONSTATE_H__
#define __OSGBULLET_MOTIONSTATE_H__

#include <osg/MatrixTransform>
#include <osg/PositionAttitudeTransform>

#include <osgSim/DOFTransform>

#include <btBulletCollisionCommon.h>

#include <osgBullet/Export.h>

namespace osgBullet {

/*!
    A btMotionState that can drive an OSG MatrixTransform (allows
    Bullet to manupulate the scene graph).
*/

class OSGBULLET_EXPORT MotionState : public btDefaultMotionState
{
public:
    MotionState( const btTransform & startTrans = btTransform::getIdentity(),
        const btTransform& centerOfMassOffset = btTransform::getIdentity() );
    ~MotionState( void ) { }

    virtual void setWorldTransform( const btTransform & centerOfMassWorldTrans );
    void setInverseParentWorldTransform( const osg::Matrix& invL2w );


    void setMatrixTransform( osg::MatrixTransform * matrixTransform )
    {
        _matrixTransform = matrixTransform;
    }
    void setDebugMatrixTransform( osg::MatrixTransform * matrixTransform )
    {
        _debugMT = matrixTransform;
    }

    osg::MatrixTransform * getMatrixTransform()
    {
        return( _matrixTransform.get() );
    }
    const osg::MatrixTransform * getMatrixTransform() const
    {
        return( _matrixTransform.get() );
    }
    osg::MatrixTransform* getDebugMatrixTransform()
    {
        return( _debugMT.get() );
    }
    const osg::MatrixTransform * getDebugMatrixTransform() const
    {
        return( _debugMT.get() );
    }

    osg::Vec3 _centerOfMass;

private:
    osg::ref_ptr< osg::MatrixTransform > _matrixTransform;
    osg::ref_ptr< osg::MatrixTransform > _debugMT;
    osg::Matrix _invL2w;
};

/*!
    For osgSim::DOFTransforms.
*/

class OSGBULLET_EXPORT DOFMotionState
    : public btDefaultMotionState
{
public:
    DOFMotionState( const btTransform & startTrans = btTransform::getIdentity(),
        const btTransform & centerOfMassOffset = btTransform::getIdentity() );
    ~DOFMotionState( void ) { }

    virtual void setWorldTransform( const btTransform & centerOfMassWorldTrans );
    void setInverseParentWorldTransform( const btTransform& invL2w );


    void setDOFTransform( osgSim::DOFTransform * dofTransform )
    {
        _dofTransform = dofTransform;
    }

    osgSim::DOFTransform * getDOFTransform()
    {
        return( _dofTransform.get() );
    }
    const osgSim::DOFTransform * getDOFTransform() const
    {
        return( _dofTransform.get() );
    }

private:
    osg::ref_ptr< osgSim::DOFTransform > _dofTransform;
};

/*!
    For osg::PositionAttitudeTransforms.
*/
class OSGBULLET_EXPORT PATMotionState
    : public btDefaultMotionState
{
public:
    PATMotionState( const btTransform & startTrans = btTransform::getIdentity(),
        const btTransform & centerOfMassOffset = btTransform::getIdentity() );
    ~PATMotionState( void ) { }

    virtual void setWorldTransform( const btTransform & centerOfMassWorldTrans );


    void setPATTransform( osg::PositionAttitudeTransform * patTransform )
    {
        _patTransform = patTransform;
    }

    osg::PositionAttitudeTransform * getPATTransform()
    {
        return( _patTransform.get() );
    }
    const osg::PositionAttitudeTransform * getPATTransform() const
    {
        return( _patTransform.get() );
    }

private:
    osg::ref_ptr< osg::PositionAttitudeTransform > _patTransform;
};

} // namespace osgBullet

#endif // __OSGBULLET_MOTIONSTATE_H__
