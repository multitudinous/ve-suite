#include "VE_Open/VE_XML/VE_CAD/CADAssembly.h"
#include <sstream>
XERCES_CPP_NAMESPACE_USE
using namespace VE_CAD;
////////////////////////////////////////////////////////////////////
CADAssembly::CADAssembly(DOMDocument* rootDocument,std::string name)
:VE_CAD::CADNode(rootDocument,name)
{
  _numChildren = 0;
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
   _numChildren = static_cast< unsigned int >(_children.size());
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
   return _numChildren; 
}
///////////////////////////////////////////////////////////////
VE_CAD::CADNode* CADAssembly::GetChild(unsigned int whichChild)
{
   return _children.at(whichChild);
}
///////////////////////////////////
void CADAssembly::_updateChildren()
{
   DOMElement* childList = _rootDocument->createElement(xercesString("children"));
   
   //the number of children
   DOMElement* nchildrenElement = _rootDocument->createElement(xercesString("numChildren"));
   std::stringstream int2string;
   int2string<<_numChildren;
   DOMText* numberOfChildren = _rootDocument->createTextNode(xercesString(int2string.str().c_str()));
   nchildrenElement->appendChild(numberOfChildren);
   _veElement->appendChild(nchildrenElement);

   //add the children nodes to the list
   for(unsigned int i = 0; i < _numChildren;  i++){
      childList->appendChild( _children.at( i )->GetXMLData("child") );
   }
   _veElement->appendChild(childList);
}
/////////////////////////////////////////////////////
void CADAssembly::_updateVEElement(std::string input)
{
   //this is going to be "nutty"
   //Get the base elements from CADNode
   VE_CAD::CADNode::_updateVEElement("CADClone");
   _updateChildren();
}
/////////////////////////////////////////////////////
void CADAssembly::SetObjectFromXMLData( DOMNode* xmlNode)
{

}
/////////////////////////////////////////////////
CADAssembly::CADAssembly(const CADAssembly& rhs)
:VE_CAD::CADNode(rhs)
{
   _numChildren = rhs._numChildren;
   for(unsigned int i = 0; i < _numChildren; i++){
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
      _numChildren = rhs._numChildren;

      for(unsigned int i =0; i < _numChildren; i++)
      {
         _children.push_back(rhs._children.at(i));
      }
   }
   return *this;
}