// Copyright 2008 Blue Newt Software LLC and Skew Matrix  Software LLC. All rights reserved.

#ifndef __OSGBULLET_HAND_NODE_H__
#define __OSGBULLET_HAND_NODE_H__


#include <osg/NodeVisitor>
#include <osgSim/DOFTransform>
#include <osg/PositionAttitudeTransform>
#include <osgBullet/Export.h>

#include <vector>
#include <map>
#include <string>


class btDynamicsWorld;
class btRigidBody;
class btCompoundShape;


namespace osgBullet {

class FindArticulations;

/*!
   HandNode -- A Node that renders a hand model in the OSG scene
   and maintains a Bullet rigid body and collision shape rep of
   the hand.
*/
class OSGBULLET_EXPORT HandNode : public osg::Transform
{
public:
    typedef enum Handedness {
        RIGHT, LEFT
    };

    HandNode( btDynamicsWorld* bulletWorld, const HandNode::Handedness rightOrLeft=HandNode::RIGHT, float handLength=HandNode::_defaultLength );
    HandNode( const HandNode& rhs, const osg::CopyOp& copyop=osg::CopyOp::SHALLOW_COPY );

    HandNode(); // Required for META_Node, but not intended for actual use.
    META_Node( osgBullet, HandNode );

    virtual void traverse( osg::NodeVisitor& nv );

    virtual osg::BoundingSphere computeBound() const;

    virtual bool computeLocalToWorldMatrix( osg::Matrix& matrix, osg::NodeVisitor* nv ) const;
    virtual bool computeWorldToLocalMatrix( osg::Matrix& matrix, osg::NodeVisitor* nv ) const;

    inline void setPosition( const osg::Vec3d& pos ) { _position = pos; dirtyBound(); updateTransform(); }
    inline const osg::Vec3d& getPosition() const { return _position; }

    inline void setAttitude( const osg::Quat& quat ) { _attitude = quat; dirtyBound(); updateTransform(); }
    inline const osg::Quat& getAttitude() const { return _attitude; }

    // Set right or left hand.
    // Set this in the constructor for maximum efficiency.
    void setHandedness( const HandNode::Handedness rightOrLeft );
    HandNode::Handedness getHandedness() const;

    // Set the desired world coordinate length of the hand. For example:
    //   about 0.15 if working in meters, or about 0.5 if working in feet.
    //   The default is about 6.5 (the hand was modeled in inches).
    void setHandLength( float length );
    float getHandLength() const;

    typedef int Articulation;
    typedef enum {
        // lateral rotation / flexure
        FINGER_0_TRANSLATE = 0, // thumb
        FINGER_1_TRANSLATE, // pointer
        FINGER_2_TRANSLATE, // middle
        FINGER_3_TRANSLATE, // ring
        FINGER_4_TRANSLATE, // pinky

        // rotation at inner knuckle
        FINGER_0_ROTATE_INNER,
        FINGER_1_ROTATE_INNER,
        FINGER_2_ROTATE_INNER,
        FINGER_3_ROTATE_INNER,
        FINGER_4_ROTATE_INNER,

        // rotation at outer knuckle
        FINGER_0_ROTATE_OUTER,
        FINGER_1_ROTATE_OUTER,
        FINGER_2_ROTATE_OUTER,
        FINGER_3_ROTATE_OUTER,
        FINGER_4_ROTATE_OUTER,

        MAX_ARTICULATIONS
    };

    void setArticulation( const HandNode::Articulation part, const float radians );
    float getArticulation( const HandNode::Articulation part ) const;

    // If set to true, angles passed to \c setArticulation are negated
    // before they are applied. This is necessary due to differences
    // between the left and right hand models, which could be
    // resolved in a future revision.
    // These methods are primarily for internal use, but made public "just in case".
    void setNegateRotation( const HandNode::Articulation part, const bool negate );
    bool getNegateRotation( const HandNode::Articulation part ) const;

    // Set to true to enable viewing a wireframr of the Bullet rep
    void setDebug( bool enable );
    bool getDebug() const;

    // Predefined positions, called "Pose" to avoid collision with base class "setPosition".
    typedef int Pose;
    typedef enum {
        POSE_DEFAULT = 0,
        POSE_HOOK,
        POSE_POINT,
        POSE_FIST
    };

    void setPose( Pose pose, float radiansPerSec=(float)(osg::PI) );


    // Dumps OSG files for children, and displays info to std::out
    //   including the current PAT matrix and the articulation angles.
    void dump() const;


    //
    // Made public for the benefit of helper NodeVisitors.
    //

    // Class to manage vuewing the hand's component collision shapes.
    class DebugBullet : public osg::Referenced
    {
    public:
        DebugBullet();
        ~DebugBullet();

        unsigned int addStatic( osg::Node* node );
        void setTransform( unsigned int idx, const osg::Matrix& m );
        osg::MatrixTransform* getRoot() const;

    protected:
        osg::ref_ptr< osg::MatrixTransform > _root;
    };

    typedef struct ArticulationInfo
    {
        ArticulationInfo();
        ~ArticulationInfo();

        void setAngle( float angle );
        float getAngle() const;

        void setBulletTransform();

        osg::ref_ptr< osgSim::DOFTransform > _dof;
        int _btChildIdx;
        int _debugIdx;

        osg::NodePath _l2wNodePath;
        osg::Vec3 _bt2osg;
        float _angle;
        bool _negate;

        ArticulationInfo* _dependent;
        btCompoundShape* _cs;

        osg::ref_ptr< HandNode::DebugBullet > _debugBullet;
    };
    typedef std::vector< ArticulationInfo > ArticulationInfoList;

protected:
    void init();

    ~HandNode();
    void cleanup();

    // Incorporate transformation changes into the Bullet collision shapes.
    void updateTransform();

    osg::ref_ptr< osg::Node > _hand;
    ArticulationInfoList _ail;
    ArticulationInfo _palm;

    Handedness _rightOrLeft;
    osg::Vec3d _position;
    osg::Quat _attitude;
    float _length;
    static float _defaultLength;

    btDynamicsWorld* _bulletWorld;
    btRigidBody* _body;
    btCompoundShape* _shape;


    // Support for debugging the Bullet rep.
    bool _debug;
    osg::ref_ptr< DebugBullet > _debugBullet;
};

} // end namespace osgBullet

#endif // __OSGBULLET_HAND_NODE_H__
