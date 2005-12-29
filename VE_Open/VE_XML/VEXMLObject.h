#ifndef _VE_XML_OBJECT_H_
#define _VE_XML_OBJECT_H_

#include <xercesc/util/XMLString.hpp>
#include <xercesc/dom/DOM.hpp>

#include <iostream>
#include <string>

#include "VE_Installer/include/VEConfig.h"
XERCES_CPP_NAMESPACE_USE
//using namespace xercesc_2_6;

namespace VE_XML
{
class VE_XML_EXPORTS VEXMLObject
{
public:
   VEXMLObject( DOMDocument* );
   virtual ~VEXMLObject(){;}
   VEXMLObject( const VEXMLObject& );
   //equal operator
   VEXMLObject& operator= ( const VEXMLObject& );
   
   virtual void SetObjectFromXMLData( DOMNode* )=0;
   DOMElement* GetXMLData( std::string ); 

   ///utility functions for reading data from an element
   ///\param element Element to extract string from.
   std::string ExtractDataStringFromSimpleElement( DOMElement* element );
   
   ///utility functions for reading data from an element
   ///\param element Element to extract double from.
   double ExtractDataNumberFromSimpleElement( DOMElement* element);
   
   ///utility functions for extracting subElement itemIndex from a complex element.
   ///\param baseElement The XML complexElement to extract a subelement from of type subElementTagName.
   ///\param subElementTagName The subelement tagname to extract from baseElement.
   ///\param itemIndex The index of the subElement to extract from the complex element.
   DOMElement* GetSubElement(DOMElement* baseElement,std::string subElementTagName,unsigned int itemIndex);
   // ---------------------------------------------------------------------------
   //  This is a simple class that lets us do easy (though not terribly efficient)
   //  trancoding of char* data to XMLCh data. --taken from xerces examples
   // ---------------------------------------------------------------------------
   class VE_XML_EXPORTS VEStr
   {
   public:
      VEStr( const char* const );
      VEStr( int );
      VEStr( double );
      VEStr( std::string );

      virtual ~VEStr();

      const XMLCh* unicodeForm( void ) const;

   private:
     // -----------------------------------------------------------------------
     //  Private data members
     //
     //  fUnicodeForm
     //      This is the Unicode XMLCh format of the string.
     // -----------------------------------------------------------------------
     XMLCh*   fUnicodeForm;
   };


protected:
   virtual void _updateVEElement( std::string )=0;
   void _clearAllChildrenFromElement();
   bool _needsUpdate;
   DOMElement* _veElement;
   DOMDocument* _rootDocument;
   unsigned int _nChildren;
};
#define xercesString(str) VEXMLObject::VEStr(str).unicodeForm()
}
#endif// _VE_XML_OBJECT_H_
