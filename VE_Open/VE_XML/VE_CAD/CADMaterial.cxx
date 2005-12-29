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
   std::vector< double > temp;
   temp.assign( 4, 1.0f );

   _kDiffuse = new VE_XML::VEFloatArray(_rootDocument);
   _kDiffuse->SetArray(temp);

   _kEmissive = new VE_XML::VEFloatArray(_rootDocument);
   _kEmissive->SetArray(temp);

   _ambient = new VE_XML::VEFloatArray(_rootDocument);
   _ambient->SetArray(temp);

   _specular = new VE_XML::VEFloatArray(_rootDocument);
   _specular->SetArray(temp);
   _shininess = 50.0;
   _materialName = name;
   _nChildren = 5;
}
///////////////////////////
//Destructor             //
///////////////////////////
CADMaterial::~CADMaterial()
{
   if(_kDiffuse)
   {
      delete _kDiffuse;
      _kDiffuse = 0;
   }
   if(_kEmissive)
   {
      delete _kEmissive;
      _kEmissive = 0;
   }
   if(_ambient)
   {
      delete _ambient;
      _ambient = 0;
   }
   if(_specular)
   {
      delete _specular;
      _specular = 0;
   }
}
/////////////////////////////////////////////////////
void CADMaterial::SetDiffuseComponent(VE_XML::VEFloatArray* diffuse)
{
   _kDiffuse = diffuse;
}
///////////////////////////////////////////////////////
void CADMaterial::SetEmissiveComponent(VE_XML::VEFloatArray* emissive)
{
   _kEmissive = emissive;
}
/////////////////////////////////////////////////////
void CADMaterial::SetAmbientComponent(VE_XML::VEFloatArray* ambient)
{
   _ambient = ambient;
}
//////////////////////////////////////////////////////
void CADMaterial::SetSpecularComponent(VE_XML::VEFloatArray* specular)
{
   _specular = specular;
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
VE_XML::VEFloatArray* CADMaterial::GetDiffuse()
{
   return _kDiffuse;
}
/////////////////////////////////////////////
VE_XML::VEFloatArray* CADMaterial::GetEmissive()
{
   return _kEmissive;
}
////////////////////////////////////////////
VE_XML::VEFloatArray* CADMaterial::GetAmbient()
{
   return _ambient;
}
/////////////////////////////////////////////
VE_XML::VEFloatArray* CADMaterial::GetSpecular()
{
   return _specular;
}
/////////////////////////////////////////////////////
void CADMaterial::_udpateShininess(std::string input)
{
   DOMElement* shineElement  = _rootDocument->createElement( xercesString("shininess") );
   _veElement->appendChild( shineElement );      
   
   DOMText* shininess = _rootDocument->createTextNode( xercesString( _shininess ) );
   shineElement->appendChild( shininess );
}
/////////////////////////////////////////////////////
void CADMaterial::_updateColorProperties(std::string input)
{
   _veElement->appendChild( _kDiffuse->GetXMLData("kDiffuse"));      
   _veElement->appendChild( _kEmissive->GetXMLData("kEmissive"));      
   _veElement->appendChild( _ambient->GetXMLData("kAmbient")); 
   _veElement->appendChild( _specular->GetXMLData("specular"));
}
/////////////////////////////////////////////////////
void CADMaterial::_updateVEElement(std::string input)
{
   VE_CAD::CADNode::_updateVEElement("CADMaterial");
   _updateColorProperties();
   _updateShininess();
}
/////////////////////////////////////////////////////
void CADMaterial::SetObjectFromXMLData( DOMNode* xmlNode)
{
   if ( xmlNode->hasChildNodes() )
   {
      // do we need to delete the old one or does xerces handle this???
      _kDiffuse->SetObjectFromXMLData( xmlNode );
      _kEmissive->SetObjectFromXMLData( xmlNode );
      _ambient->SetObjectFromXMLData( xmlNode );
      _specular->SetObjectFromXMLData(xmlNode);
   }
}
////////////////////////////////////////////////
CADMaterial::CADMaterial(const CADMaterial& rhs)
:VEXMLObject(rhs)
{
   _kDiffuse = new VE_XML::VEFloatArray(*rhs._kDiffuse);
   _kEmissive = new VE_XML::VEFloatArray(*rhs._kEmissive);
   _ambient = new VE_XML::VEFloatArray(*rhs._ambient);
   _specular = new VE_XML::VEFloatArray(*rhs._specular);
   _shininess = rhs._shininess;
   _materialName = rhs._materialName;
}
////////////////////////////////////////////////////////////
CADMaterial& CADMaterial::operator=(const CADMaterial& rhs)
{
   if ( this != &rhs )
   {
      if(_kDiffuse)
      {
         delete _kDiffuse;
         _kDiffuse = 0;
      }
      if(_kEmissive)
      {
         delete _kEmissive;
         _kEmissive = 0;
      }
      if(_ambient)
      {
         delete _ambient;
         _ambient = 0;
      }
      if(_specular)
      {
         delete _specular;
         _specular = 0;
      }
      _kDiffuse = new VE_XML::VEFloatArray(*rhs._kDiffuse);
      _kEmissive = new VE_XML::VEFloatArray(*rhs._kEmissive);
      _ambient = new VE_XML::VEFloatArray(*rhs._ambient);
      _specular = new VE_XML::VEFloatArray(*rhs._specular);
      _shininess = rhs._shininess;
      _materialName = rhs._materialName;
   }
   return *this;
}