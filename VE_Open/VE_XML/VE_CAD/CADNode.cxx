#include "VE_Open/VE_XML/VETransform.h"
#include "VE_Open/VE_XML/VE_CAD/CADAssembly.h"
#include "VE_Open/VE_XML/VE_CAD/CADMaterial.h"
XERCES_CPP_NAMESPACE_USE
using namespace VE_CAD;
//////////////////////////////////
///Constructor                  //
//////////////////////////////////
CADNode::CADNode(DOMDocument* rootDoc,
                 std::string name)
:VE_XML::VEXMLObject(rootDoc)
{
   _name = name;
   _parent = 0;
   _transform = new VE_XML::VETransform(_rootDocument); 
   _material = new VE_CAD::CADMaterial(_rootDocument); 
}
///////////////////
///Destructor    //
///////////////////
CADNode::~CADNode()
{
   if(_transform){
      delete _transform;
      _transform = 0;
   }
   if(_material){
      delete _material;
      _material = 0;
   }
}
///////////////////////////////////////////
void CADNode::SetNodeName(std::string name)
{
   _name = name;
}
////////////////////////////////////////////////////
void CADNode::SetParent(VE_CAD::CADAssembly* parent)
{
   _parent = parent;
}
///////////////////////////////////////////////////////
void CADNode::SetTransform(VE_XML::VETransform* transform)
{
   if(_transform)
   {
      delete _transform;
      _transform = 0;
   }
   _transform = new VE_XML::VETransform(*transform);
}
////////////////////////////////////////////////////////
void CADNode::SetMaterial(VE_CAD::CADMaterial* material)
{
   if(_material)
   {
      delete _material;
      _material = 0;
   }
   _material = new CADMaterial(*material);
}
//////////////////////////////////
std::string CADNode::GetNodeName()
{
   return _name;
}
/////////////////////////////////////////
VE_CAD::CADAssembly* CADNode::GetParent()
{
   return _parent;
}
//////////////////////////////////////////
VE_XML::VETransform* CADNode::GetTransform()
{
   return _transform;
}
///////////////////////////////////////////
VE_CAD::CADMaterial* CADNode::GetMaterial()
{
   return _material;
}
/////////////////////////////////////////////////
void CADNode::_updateVEElement(std::string input)
{
   //how is this going to work???
}
/////////////////////////////////////////////////////
void CADNode::SetObjectFromXMLData( DOMNode* xmlNode)
{
}
/////////////////////////////////////
CADNode::CADNode(const CADNode& rhs)
:VE_XML::VEXMLObject(rhs)
{
   _transform = new VE_XML::VETransform(*rhs._transform);
   _material = new VE_CAD::CADMaterial(*rhs._material);
   _parent = rhs._parent;
   _name = rhs._name;
}
////////////////////////////////////////////////
CADNode& CADNode::operator=(const CADNode& rhs)
{
   if ( this != &rhs )
   {
      if(_transform)
      {
         delete _transform;
         _transform = 0;
      }
      _transform = new VE_XML::VETransform(*rhs._transform);
      if(_material)
      {
         delete _material;
         _material = 0;
      }
      _material = new VE_CAD::CADMaterial(*rhs._material);
      _parent = rhs._parent;
      _name = rhs._name;
   }
   return *this;
}