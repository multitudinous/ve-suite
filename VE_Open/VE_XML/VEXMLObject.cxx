#include "VE_Open/VE_XML/VEXMLObject.h"
#include <sstream>
#include <iomanip>
XERCES_CPP_NAMESPACE_USE

using namespace VE_XML;
//////////////////////////
VEXMLObject::VEXMLObject(DOMDocument* rootDoc)
{
   _veElement = 0;
   _needsUpdate = false;
   _rootDocument = rootDoc; 
   _nChildren = 0;
}
///////////////////////////////////////////
VEXMLObject::VEXMLObject( const VEXMLObject& input )
{
   _veElement = input._veElement;
   _needsUpdate = input._needsUpdate;
   _rootDocument = input._rootDocument; 
   _nChildren = input._nChildren;
}
/////////////////////////////////////////////////////
VEXMLObject& VEXMLObject::operator=( const VEXMLObject& input)
{
   if ( this != &input )
   {
      _veElement = input._veElement;
      _needsUpdate = input._needsUpdate;
      _rootDocument = input._rootDocument; 
      _nChildren = input._nChildren;
   }
   return *this;
}
/////////////////////////////////////
DOMElement* VEXMLObject::GetXMLData( std::string input )
{
   //Make sure old data is cleared from the xerces side of the element
   _clearAllChildrenFromElement();
  
   //update the xerces element w/ the current data in the object
   //This function should be overridden in ALL derived classes!!!!!
   _updateVEElement( input );

   return _veElement;
}
////////////////////////////////////////////////
void VEXMLObject::_clearAllChildrenFromElement()
{
   if ( _veElement )
   {
      for(unsigned int i = _nChildren - 1; i > -1; i--)
      {
         _veElement->removeChild(_veElement->getChildNodes()->item(i));
      }
   }
}
///////////////////////////////////////////
VEXMLObject::VEStr::VEStr(const char* const toTranscode)
{
   // Call the private transcoding method
   fUnicodeForm = XMLString::transcode(toTranscode);
}
///////////////////////////////////////////
VEXMLObject::VEStr::VEStr( std::string input )
{
   // Call the private transcoding method
   fUnicodeForm = XMLString::transcode( input.c_str() );
}
///////////////////////////////////////////
VEXMLObject::VEStr::VEStr( int input )
{
   std::ostringstream dirStringStream;
   dirStringStream << std::setprecision(10) << input;

   // Call the private transcoding method
   fUnicodeForm = XMLString::transcode( dirStringStream.str().c_str() );
}
/////////////////////////////////////////
VEXMLObject::VEStr::VEStr( double input )
{
   std::ostringstream dirStringStream;
   dirStringStream << std::setprecision(10) << input;

   // Call the private transcoding method
   fUnicodeForm = XMLString::transcode(dirStringStream.str().c_str());
}
////////////////////////////
VEXMLObject::VEStr::~VEStr()
{
   XMLString::release(&fUnicodeForm);
}
////////////////////////////////////////////////////
const XMLCh* VEXMLObject::VEStr::unicodeForm() const
{
   return fUnicodeForm;
}
///////////////////////////////////////////////////////////////////////////
std::string VEXMLObject::ExtractDataStringFromSimpleElement(DOMElement* element)
{
   return std::string(XMLString::transcode(element->getNodeValue()));
}
//////////////////////////////////////////////////////////////////////////
double VEXMLObject::ExtractDataNumberFromSimpleElement(DOMElement* element)
{
   return atof(std::string(XMLString::transcode(element->getNodeValue())).c_str());
}
