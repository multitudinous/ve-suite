#ifndef FILE_H
#define FILE_H

#include <vector>
#include <string>

#include "VE_Installer/include/VEConfig.h"

#include "VE_Xplorer/SceneGraph/DCS.h"
#include "VE_Xplorer/SceneGraph/Node.h"

namespace opal
{
   class Solid;
}

#ifdef _PERFORMER
   class pfFog;
#elif _OSG
   namespace osg
   {
      class Fog;
   }
#endif

namespace VE_SceneGraph
{
class VE_SCENEGRAPH_EXPORTS FILE 
{
public:
   FILE(std::string, VE_SceneGraph::DCS*, bool isStream=false);
   ~FILE();

   void Initialize(float);

   //void pfTravNodeMaterial(pfNode*, pfMaterial*, int);

   VE_SceneGraph::DCS* GetDCS();
   VE_SceneGraph::Node* GetNode();

   opal::Solid* GetSolid();

   void SetFILEProperties(int, int, float*);
   int GetTransparentFlag();
   void setOpac(float op_val);
   float getOpacity();
   void setFog(double dist);

   std::string GetFilename();
   //pfLightModel *matLight;
   //pfMaterial *fmaterial;
   //pfMaterial *bmaterial;
   //std::vector<pfMaterial*> matList;
   void SetRGBAColorArray(double*);
   void GetColorArray();
   void SetGeometryFilename(std::string);
   void SetModuleName(std::string);
   void SetTransparencyFlag(bool);
   void SetColorFlag(int);
   int GetColorFlag();
   std::string GetModuleName();
   void SetColorOfGeometry(VE_SceneGraph::Node*);
   void Update();
   void SetOpacity(float);

private:
      //These should be private, I would think
      osg::ref_ptr< VE_SceneGraph::Node > node;
   osg::ref_ptr< VE_SceneGraph::DCS > DCS;
   ////////////////////////////////////////
   
   //pfMaterial *mat1, *mat0;
   int mat_count;
   int color;
   int transparent;
   float stlColor[3];
   //char* fileName;
   std::string fileName;//[512];
      
      float op;

   #ifdef _PERFORMER
      pfFog* fog;
   #elif _OSG
      osg::Fog* fog;
   #endif

   double _rgba[4];
   bool _transparencyFlag;
   float _opacityLevel;
   int _colorFlag;
   osg::ref_ptr< VE_SceneGraph::Node > _node;
   //Group* _masterNode;
   std::string _filename;
   std::string _moduleName;

   opal::Solid* solid;
};
}

#endif

