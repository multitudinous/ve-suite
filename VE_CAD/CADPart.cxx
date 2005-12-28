#include "VE_CAD/CADPart.h"
XERCES_CPP_NAMESPACE_USE
////////////////////////////////////////////////////////////
//Constructor                                             //
////////////////////////////////////////////////////////////
CADPart::CADPart(DOMDocument* rootDocument,std::string name)
:CADPart(rootDocument, name)
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
////////////////////////////////
void CADPart::_updateVEElement()
{
}
