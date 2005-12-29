#include "VE_Open/VE_XML/VE_CAD/CADPart.h"
XERCES_CPP_NAMESPACE_USE
using namespace VE_CAD;
////////////////////////////////////////////////////////////
//Constructor                                             //
////////////////////////////////////////////////////////////
CADPart::CADPart(DOMDocument* rootDocument,std::string name)
:VE_CAD::CADNode(rootDocument,name)
{
   _cadFileName = std::string("CADFile");
}
///////////////////
//Destructor     //
///////////////////
CADPart::~CADPart()
{
}
/////////////////////////////////////////////////////
void CADPart::SetCADFileName(std::string cadFileName)
{
   _cadFileName = cadFileName;
}
/////////////////////////////////////
std::string CADPart::GetCADFileName()
{
   return _cadFileName;
}
//////////////////////////////////
void CADPart::_updateCADFileName()
{
   DOMElement* nameElement  = _rootDocument->createElement( xercesString("filename") );
   _veElement->appendChild( nameElement );      
   
   DOMText* fileName = _rootDocument->createTextNode( xercesString( _cadFileName ) );
   nameElement->appendChild( fileName  );
}
/////////////////////////////////////////////////
void CADPart::_updateVEElement(std::string input)
{
   //How is this going to work???
   //Get the base elements from CADNode
   VE_CAD::CADNode::_updateVEElement("CADPart");
   _updateCADFileName();
}
/////////////////////////////////////////////////////
void CADPart::SetObjectFromXMLData( DOMNode* xmlNode)
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
            DOMElement* fileNameElement = dynamic_cast<DOMElement*>(currentElement->getElementsByTagName(xercesString("fileName"))->item(0));
            _cadFileName = ExtractDataStringFromSimpleElement(fileNameElement);
         }
      }
   }
}
/////////////////////////////////////
CADPart::CADPart(const CADPart& rhs)
:VE_CAD::CADNode(rhs)
{
   _cadFileName = rhs._cadFileName;
}
////////////////////////////////////////////////
CADPart& CADPart::operator=(const CADPart& rhs)
{
   if ( this != &rhs )
   {
      VE_CAD::CADNode::operator =(rhs);
      _cadFileName = rhs._cadFileName;
   }
   return *this;
}