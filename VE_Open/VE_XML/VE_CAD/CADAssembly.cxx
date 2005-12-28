#include "VE_CAD/CADAssembly.h"
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
}
//////////////////////////////////////////////////////
bool CADAssembly::RemoveChild(unsigned int whichChild) 
{
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
