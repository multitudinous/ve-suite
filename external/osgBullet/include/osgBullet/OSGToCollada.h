// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
// All rights reserved.

#ifndef __OSGBULLET_OSG_TO_COLLADA_H__
#define __OSGBULLET_OSG_TO_COLLADA_H__ 1


#include <osg/NodeVisitor>
#include <btBulletDynamicsCommon.h>
#include <osgBullet/CollisionShapes.h>
#include <osgBullet/Export.h>
//#include <osgBullet/CollisionShapes.h>
#include <string>

namespace osgBullet {


// This is a support class for the osgbpp application, so that
// an app can programmatically do the same thing as osgbpp:
// Namely, convert an input scene graph into COLLADA physics data.
class OSGBULLET_EXPORT OSGToCollada
{
public:
    // NOTR: loadedModel is _not_ const. This _will_ alter your scene graph.
    // It flattens static transforms using the osgUtil::Optimizer.
    // It removes loaded ProxyNodes (again with the Optimizer).
    // Polygon reduction is performed if:
    //   * _simplifiyPercenr or _decimatorPercent are != 1.0
    //   * _vertexAggMaxVerts is positive.
    //
    // 'overall' and 'nodeName' work together to determine how the Bullet collision
    // shape is created (which is used to create the rigid body that is written to
    // the Collada file):
    //   overall  nodeName   Results
    //    true     empty     A single collision shape is created based on the bounding volume of the entire 'sg' scene graph.
    //    true    non-empty  'sg' is searched until a node with name 'nodeName' is found, then a single collision shape is created based on the bounding volume of the subgraph rooted at that node.
    //    false    empty     Collision shapes are created for all Geodes in 'sg' and assembled into a compound collision shape.
    //    false   non-empty  'sg' is searched until a node with name 'nodeName' is found, then collision shapes are created for all Geodes below that node and assembled into a compound collision shape.
    // Typical expected usage is 'overall' false and 'nodeName' empty.
    OSGToCollada();

    bool convert( const std::string& outputFileName = std::string( "" ) );


    btRigidBody* getRigidBody();

    void setSceneGraph( osg::Node* sg );
    osg::Node* getSceneGraph() const;

    // Specifying the center of mass automatically disables auto-compute center of mass.
    void setCenterOfMass( osg::Vec3& com );
    const osg::Vec3& getCenterOfMass() const;
    void setAutoComputeCenterOfMass( bool compute );
    bool getAutoComputeCenterOfMass() const;

    void setShapeType( const BroadphaseNativeTypes shapeType );
    BroadphaseNativeTypes getShapeType() const;

    void setMass( float mass );
    float getMass() const;

    void setDecimateParamaters( float decimatePercent, float maxError=FLT_MAX );

    void setSimplifyPercent( float simplifyPercent );
    float getSimplifyPercent() const;

    void setVertexAggParameters( unsigned int maxVertsPerCell, osg::Vec3 minCellSize=osg::Vec3(0.,0.,0.) );

    void setOverall( bool overall );
    bool getOverall() const;

    void setNodeName( const std::string& nodeName );
    const std::string& getNodeName() const;

    // for cylinder alignment only, ignored otherwise.
    void setAxis( osgBullet::AXIS axis );
    osgBullet::AXIS getAxis() const;

protected:
    btDynamicsWorld* initPhysics();
    btRigidBody* createRigidBody( btScalar mass,
        btCollisionShape* shape, const osg::Vec3& centerOfMass, osg::Node* node );


    btRigidBody* _rigidBody;

    osg::ref_ptr< osg::Node > _sg;
    osg::Vec3 _com;
    bool _comSet;
    BroadphaseNativeTypes _shapeType;
    float _mass;

    float _decimatorPercent;
    float _decimatorMaxError;
    float _simplifyPercent;
    unsigned int _vertexAggMaxVerts;
    osg::Vec3 _vertexAggMinCellSize;

    bool _overall;
    std::string _nodeName;
    osgBullet::AXIS _axis;
};

}

#endif
