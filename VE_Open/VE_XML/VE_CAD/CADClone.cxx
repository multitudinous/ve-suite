#include "VE_Open/VE_XML/VE_CAD/CADClone.h"
XERCES_CPP_NAMESPACE_USE
using namespace VE_CAD;
//////////////////////////////////////////////////////////////////
//Constructor                                                   //
//////////////////////////////////////////////////////////////////
CADClone::CADClone(DOMDocument* rootDocument,std::string name,
                 VE_CAD::CADNode* originalNode)
:VE_CAD::CADNode(rootDocument,name)
{
   _originalNode = originalNode;
   _type = std::string("Clone");
}
/////////////////////
//Destructor       //
/////////////////////
CADClone::~CADClone()
{
}
//////////////////////////////////////////////////
void CADClone::_updateVEElement(std::string input)
{
   //How is this going to work???
   //Get the base elements from CADNode
   VE_CAD::CADNode::_updateVEElement("CADClone");

   //add the extra stuff
   _veElement->appendChild(_originalNode->GetXMLData("originalNode"));
   
}
/////////////////////////////////////////////////////
void CADClone::SetObjectFromXMLData( DOMNode* xmlNode)
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

      //break down the element
      {
         if(currentElement->hasChildNodes())
         {
            DOMElement* originalNode = GetSubElement(currentElement,std::string("originalNode"),0);
            _originalNode->SetObjectFromXMLData(originalNode);
         }
      }
   }
}
///////////////////////////////////////
CADClone::CADClone(const CADClone& rhs)
:VE_CAD::CADNode(rhs)
{
   _originalNode = rhs._originalNode;
}
///////////////////////////////////////////////////
CADClone& CADClone::operator=(const CADClone& rhs)
{
   if ( this != &rhs )
   {
      VE_CAD::CADNode::operator =(rhs);
      _originalNode = rhs._originalNode;
   }
   return *this;
}