#ifndef PHYSICS_MESH_H
#define PHYSICS_MESH_H

#include <osg/ref_ptr>
#include <osg/NodeVisitor>
#include <osg/BoundingBox>

class btTriangleMesh;
class btCollisionShape;

class PhysicsMesh : public osg::NodeVisitor
{
public:
	PhysicsMesh( osg::Node* osg_node );
	virtual ~PhysicsMesh();

	virtual void apply( osg::Geode& geode );

	void CreateBBMesh();
	void CreateExactMesh();

	btCollisionShape* GetBBMesh();
	btCollisionShape* GetExactMesh();

private:
	osg::BoundingBox bb;
	btTriangleMesh* triMesh;

	btCollisionShape* collision_shape_bb;
	btCollisionShape* collision_shape_exact;
};

#endif //PHYSICS_MESH_H
