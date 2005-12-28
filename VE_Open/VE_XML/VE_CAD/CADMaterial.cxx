#include "VE_Open/VE_XML/VE_CAD/CADMaterial.h"
#include "VE_Open/VE_XML/VEFloatArray.h"
XERCES_CPP_NAMESPACE_USE
using namespace VE_CAD;
////////////////////////////////////////////////////////////////////
//Constructor                                                     //
////////////////////////////////////////////////////////////////////
CADMaterial::CADMaterial(DOMDocument* rootDocument,std::string name)
:VE_XML::VEXMLObject(rootDocument)
{
   _kDiffuse.assign(4,1.0f);
   _kEmissive.assign(4,1.0f);
   _ambient.assign(4,1.0f);
   _specular.assign(4,1.0f);
   _shininess = 50.0;
   _materialName = name;
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
   _kDiffuse.clear();
   _kDiffuse.push_back(diffuse[0]);
   _kDiffuse.push_back(diffuse[1]);
   _kDiffuse.push_back(diffuse[2]);
   _kDiffuse.push_back(diffuse[3]);
}
///////////////////////////////////////////////////////
void CADMaterial::SetEmissiveComponent(float* emissive)
{
   _kEmissive.clear();
   _kEmissive.push_back(emissive[0]);
   _kEmissive.push_back(emissive[1]);
   _kEmissive.push_back(emissive[2]);
   _kEmissive.push_back(emissive[3]);
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
   return _kDiffuse;
}
/////////////////////////////////////////////
std::vector<float> CADMaterial::GetEmissive()
{
   return _kEmissive;
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
/////////////////////////////////////////////////////
void CADMaterial::SetObjectFromXMLData( DOMNode* xmlNode)
{
}
////////////////////////////////////////////////
CADMaterial::CADMaterial(const CADMaterial& rhs)
:VEXMLObject(rhs)
{
   
   for(unsigned int i = 0; i < 4; i++){
      _kDiffuse.push_back(rhs._kDiffuse.at(i));
      _kEmissive.push_back(rhs._kEmissive.at(i));
      _ambient.push_back(rhs._ambient.at(i));
      _specular.push_back(rhs._specular.at(i));
   }
   _shininess = rhs._shininess;
   _materialName = rhs._materialName;
}
////////////////////////////////////////////////////////////
CADMaterial& CADMaterial::operator=(const CADMaterial& rhs)
{
   if ( this != &rhs )
   {
      _kDiffuse.clear();
      _kEmissive.clear();
      _ambient.clear();
      _specular.clear();

      for(unsigned int i = 0; i < 4; i++){
         _kDiffuse.push_back(rhs._kDiffuse.at(i));
         _kEmissive.push_back(rhs._kEmissive.at(i));
         _ambient.push_back(rhs._ambient.at(i));
         _specular.push_back(rhs._specular.at(i));
      }
      _shininess = rhs._shininess;
      _materialName = rhs._materialName;
   }
   return *this;
}