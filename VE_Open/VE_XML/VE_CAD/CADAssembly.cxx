#include "VE_Open/VE_XML/VE_CAD/CADAssembly.h"
#include "VE_Open/VE_XML/VE_CAD/CADPart.h"
#include "VE_Open/VE_XML/VE_CAD/CADClone.h"
#include <sstream>
XERCES_CPP_NAMESPACE_USE
using namespace VE_CAD;
////////////////////////////////////////////////////////////////////
CADAssembly::CADAssembly(DOMDocument* rootDocument,std::string name)
:VE_CAD::CADNode(rootDocument,name)
{
  _numChildren = 0;
  _type = std::string("Assembly");
}
///////////////////////////
///Destructor            //
///////////////////////////
CADAssembly::~CADAssembly()
{
   for(unsigned int i = _numChildren -1; i >=0; i--)
   {
      delete _children.at(i);
   }
   _children.clear();
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
   std::cout<<"CADAssembly::RemoveChild() not implemented yet!!!"<<std::endl;
   return false;
}
//////////////////////////////////////////////////////
bool CADAssembly::RemoveChild(unsigned int whichChild) 
{
   std::cout<<"CADAssembly::RemoveChild() not implemented yet!!!"<<std::endl;
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
   DOMElement* currentElement = 0;

   if(xmlNode->getNodeType() == DOMNode::ELEMENT_NODE)
   {
      currentElement = dynamic_cast<DOMElement*>(xmlNode);
   }
   
   if(currentElement)
   {
      //populate the base elements in node
      VE_CAD::CADNode::SetObjectFromXMLData(xmlNode);

      //clear out the current list of children
      for(unsigned int i = _numChildren -1; i >=0; i--)
      {
         delete _children.at(i);
      }
      _children.clear();
      //get the new number of children
      {
          DOMElement* nChildrenElement = GetSubElement(currentElement,std::string("numChildren"),0);
          _numChildren = static_cast<int>(ExtractDataNumberFromSimpleElement(nChildrenElement));
      }
      //populate the childList
      {
         DOMNodeList* childList = currentElement->getElementsByTagName(xercesString("children"));
         for(unsigned int i = 0; i < _numChildren; i++)
         {
            DOMElement* cadNode = dynamic_cast<DOMElement*>(childList->item(i));
            DOMElement* nodeType = GetSubElement(cadNode,std::string("type"),0);
            if(ExtractDataStringFromSimpleElement(nodeType) == std::string("Assembly"))
            {
               //this is an Assembly
               VE_CAD::CADAssembly* newAssembly = new VE_CAD::CADAssembly(_rootDocument);
               newAssembly->SetObjectFromXMLData(cadNode);
               _children.push_back(newAssembly);
            }else if(ExtractDataStringFromSimpleElement(nodeType) == std::string("Part")){
               //this is a Part
               VE_CAD::CADPart* newPart = new VE_CAD::CADPart(_rootDocument);
               newPart->SetObjectFromXMLData(cadNode);
               _children.push_back(newPart);
            }else if(ExtractDataStringFromSimpleElement(nodeType) == std::string("Clone")){
               //this is a Clone
               VE_CAD::CADClone* newClone = new VE_CAD::CADClone(_rootDocument);
               newClone->SetObjectFromXMLData(cadNode);
               _children.push_back(newClone);
            }else{
               std::cout<<"ERROR!"<<std::endl;
               std::cout<<"Unknown node type:"<<ExtractDataStringFromSimpleElement(nodeType)<<std::endl;    
            }
         }
      }
   }
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