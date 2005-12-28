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
/////////////////////////////////////////////////
void CADPart::_updateVEElement(std::string input)
{
}
/////////////////////////////////////////////////////
void CADPart::SetObjectFromXMLData( DOMNode* xmlNode)
{
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