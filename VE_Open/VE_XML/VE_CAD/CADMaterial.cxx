#include "VE_CAD/CADMaterial.h"
#include "VE_XML/VEFloatArray.h"
XERCES_CPP_NAMESPACE_USE
using namespace VE_CAD;
////////////////////////////////////////////////////////////////////
//Constructor                                                     //
////////////////////////////////////////////////////////////////////
CADMaterial::CADMaterial(DOMDocument* rootDocument,std::string name)
{
   _kDiffuse.assign(4,1.0f);
   _kEmission.assign(4,1.0f);
   _ambient.assign(4,1.0f);
   _specular.assign(4,1.0f)
   _shininess = 50.0;
}
///////////////////////////
//Destructor             //
///////////////////////////
CADMaterial::~CADMaterial()
{
}
/////////////////////////////////////////////////////
void CADMaterial::SetDiffuseComponent(float* diffuse)
{
   _diffuse.clear();
   _diffuse.push_back(diffuse[0]);
   _diffuse.push_back(diffuse[1]);
   _diffuse.push_back(diffuse[2]);
   _diffuse.push_back(diffuse[3]);
}
///////////////////////////////////////////////////////
void CADMaterial::SetEmissiveComponent(float* emissive)
{
   _emissive.clear();
   _emissive.push_back(emissive[0]);
   _emissive.push_back(emissive[1]);
   _emissive.push_back(emissive[2]);
   _emissive.push_back(emissive[3]);
}
/////////////////////////////////////////////////////
void CADMaterial::SetAmbientComponent(float* ambient)
{
   _ambient.clear();
   _ambient.push_back(ambient[0]);
   _ambient.push_back(ambient[1]);
   _ambient.push_back(ambient[2]);
   _ambient.push_back(ambient[3]);
}
//////////////////////////////////////////////////////
void CADMaterial::SetSpecularComponent(float* specular)
{
   _specular.clear();
   _specular.push_back(specular[0]);
   _specular.push_back(specular[1]);
   _specular.push_back(specular[2]);
   _specular.push_back(specular[3]);
}
///////////////////////////////////////////////
void CADMaterial::SetShininess(float shininess)
{
   _shininess = shininess;
}
/////////////////////////////////
float CADMaterial::GetShininess()
{
   return _shininess;
}
////////////////////////////////////////////
std::vector<float> CADMaterial::GetDiffuse()
{
   return _diffuse;
}
/////////////////////////////////////////////
std::vector<float> CADMaterial::GetEmissive()
{
   return _emissive;
}
////////////////////////////////////////////
std::vector<float> CADMaterial::GetAmbient()
{
   return _ambient;
}
/////////////////////////////////////////////
std::vector<float> CADMaterial::GetSpecular()
{
   return _specular;
}
/////////////////////////////////////////////////////
void CADMaterial::_updateVEElement(std::string input)
{
}
