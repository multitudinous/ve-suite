#ifndef CAD_ENTITY_H
#define CAD_ENTITY_H

#include <vector>
#include <string>

#include "VE_Installer/include/VEConfig.h"

#include "VE_Xplorer/SceneGraph/DCS.h"
#include "VE_Xplorer/SceneGraph/Node.h"

#ifdef _PERFORMER
class pfFog;
#elif _OSG
namespace osg
{
   class Fog;
}
#endif

class btRigidBody;

namespace VE_SceneGraph
{
class VE_SCENEGRAPH_EXPORTS CADEntity
{
public:
   CADEntity(std::string,VE_SceneGraph::DCS*,bool isStream=false);
   ~CADEntity();

   void Initialize(float);

   VE_SceneGraph::DCS* GetDCS();
   VE_SceneGraph::Node* GetNode();
   btRigidBody* GetRigidBody();

   std::string GetFilename();
   std::string GetModuleName();
   void GetColorArray();
   int GetTransparentFlag();
   int GetColorFlag();
   float getOpacity();

   void UpdateMatTransform();

   void SetFILEProperties(int,int,float*);
   void setOpac(float op_val);
   void setFog(double dist);
   void SetRGBAColorArray(double*);
   void SetGeometryFilename(std::string);
   void SetModuleName(std::string);
   void SetTransparencyFlag(bool);
   void SetColorFlag(int);
   void SetColorOfGeometry(VE_SceneGraph::Node*);
   void SetOpacity(float);
   
   void Update();

   //void pfTravNodeMaterial(pfNode*, pfMaterial*, int);
   
   //pfLightModel *matLight;
   //pfMaterial *fmaterial;
   //pfMaterial *bmaterial;
   //std::vector<pfMaterial*> matList;

private:
   //Group* _masterNode;
   osg::ref_ptr<VE_SceneGraph::Node> node;
   osg::ref_ptr<VE_SceneGraph::DCS> DCS;
   btRigidBody* rigid_body;

   //pfMaterial *mat1, *mat0;
   int mat_count;
   int color;
   int transparent;
   int _colorFlag;

   float stlColor[3];
   float op;
   float _opacityLevel;

   double _rgba[4];

   bool _transparencyFlag;

   //char* fileName;
   std::string fileName;//[512];
   std::string _filename;
   std::string _moduleName;

   #ifdef _PERFORMER
      pfFog* fog;
   #elif _OSG
      osg::Fog* fog;
   #endif
};
}

#endif //CAD_ENTITY_H

