#include "VE_Open/VE_XML/VE_CAD/CADAssembly.h"
XERCES_CPP_NAMESPACE_USE
using namespace VE_CAD;
////////////////////////////////////////////////////////////////////
CADAssembly::CADAssembly(DOMDocument* rootDocument,std::string name)
:VE_CAD::CADNode(rootDocument,name)
{
  _nChildren = 0;
}
///////////////////////////
///Destructor            //
///////////////////////////
CADAssembly::~CADAssembly()
{
}
/////////////////////////////////////////////////
void CADAssembly::AddChild(VE_CAD::CADNode* node)
{
   _children.push_back(node);
   _nChildren = _children.size();
}
////////////////////////////////////////////////////
bool CADAssembly::RemoveChild(VE_CAD::CADNode* node)
{
   return false;
}
//////////////////////////////////////////////////////
bool CADAssembly::RemoveChild(unsigned int whichChild) 
{
   return false;
}
///////////////////////////////////////////////
unsigned int CADAssembly::GetNumberOfChildren()
{
   return _nChildren; 
}
///////////////////////////////////////////////////////////////
VE_CAD::CADNode* CADAssembly::GetChild(unsigned int whichChild)
{
   return _children.at(whichChild);
}
/////////////////////////////////////////////////////
void CADAssembly::_updateVEElement(std::string input)
{
   //this is going to be "nutty"
}
/////////////////////////////////////////////////////
void CADAssembly::SetObjectFromXMLData( DOMNode* xmlNode)
{
}
/////////////////////////////////////////////////
CADAssembly::CADAssembly(const CADAssembly& rhs)
:VE_CAD::CADNode(rhs)
{
   _nChildren = rhs._nChildren;
   for(unsigned int i = 0; i < _nChildren; i++){
      _children.push_back(rhs._children.at(i));
   }
}
///////////////////////////////////////////////////////////
CADAssembly& CADAssembly::operator=(const CADAssembly& rhs)
{
   if ( this != &rhs )
   {
      VE_CAD::CADNode::operator =(rhs);
      _children.clear();
      _nChildren = rhs._nChildren;

      for(unsigned int i =0; i < _nChildren; i++)
      {
         _children.push_back(rhs._children.at(i));
      }
   }
   return *this;
}