#ifndef CAD_ENTITY_H
#define CAD_ENTITY_H

#include <vector>
#include <string>

#include "VE_Installer/include/VEConfig.h"


//Should not have to include these here
#include "VE_Xplorer/SceneGraph/DCS.h"
#include "VE_Xplorer/SceneGraph/PhysicsMesh.h"

namespace VE_SceneGraph
{
	class DCS;
	class CADEntityHelper;
}

#ifdef _OSG
#include <osg/ref_ptr>
namespace osg
{
   class Fog;
}
#include <osg/Fog>
#endif

class PhysicsMesh;

class btRigidBody;
class btCollisionShape;

namespace VE_SceneGraph
{
class VE_SCENEGRAPH_EXPORTS CADEntity
{
public:
	CADEntity( std::string, VE_SceneGraph::DCS*, bool isStream = false );
   ~CADEntity();

	VE_SceneGraph::DCS* GetDCS();
   VE_SceneGraph::CADEntityHelper* GetNode();
   btRigidBody* GetRigidBody();

	void SetMass( float m );
	void SetFriction( float f );
	void SetRestitution( float r );

	void SetPhysics( bool p );
	void SetExactShape();

   std::string GetFilename();
   std::string GetModuleName();
   void GetColorArray();
   int GetTransparentFlag();
   int GetColorFlag();
   float getOpacity();

   void SetFILEProperties( int, int, float* );
   void setOpac( float op_val );
   void setFog( double dist );
   void SetRGBAColorArray( double* );
   void SetGeometryFilename( std::string );
   void SetModuleName( std::string );
   void SetTransparencyFlag( bool );
   void SetColorFlag( int );
   //void SetColorOfGeometry( VE_SceneGraph::Node* );
   void SetOpacity( float );

	void Update();

private:
	void Initialize( float );

	VE_SceneGraph::CADEntityHelper* node;
	osg::ref_ptr< VE_SceneGraph::DCS > dcs;
   btRigidBody* rigid_body;
	osg::ref_ptr< PhysicsMesh > physics_mesh;
	btCollisionShape* collision_shape;

	float mass;
	float friction;
	float restitution;

	bool physics;

   int mat_count;
   int color;
   int transparent;
   int _colorFlag;

   float stlColor[3];
   float op;
   float _opacityLevel;

   double _rgba[4];

   bool _transparencyFlag;

   std::string fileName;		//[512];
   std::string _filename;
   std::string _moduleName;

#ifdef _OSG
   osg::Fog* fog;
#endif

};
}

#endif //CAD_ENTITY_H
