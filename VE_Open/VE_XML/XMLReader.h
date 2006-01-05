#ifndef XML_FILE_READER_H
#define XML_FILE_READER_H

/*!\file XMLReader.h
  XMLReader API
  */

/*!\class XMLReader
 *Abstract class to set things up for reading VE-Suite XML files .
 *Returns a scene node representing the file read in.
 * This class is the base class for representing
 * the hierarchy of a CAD structure.
 */

namespace VE_Conductor
{
   class DOMDocumentManager;
}
#include <xercesc/dom/DOM.hpp>
#include <string>
#include "VE_Installer/include/VEConfig.h"

XERCES_CPP_NAMESPACE_USE

namespace VE_XML{
class VE_XML_EXPORTS XMLReader{
public:
   ///Default Constructor
   XMLReader();
   
   ///Destructor
   virtual ~XMLReader();

   ///Read an input XML string of type.
   ///\param xmlString The input XML string.
   ///\param iType The input XML type.
   virtual void ReadXMLData(std::string xmlData);

   ///Set the Active DOMDocumentManager
   void SetDOMDocumentManager(VE_Conductor::DOMDocumentManager* ddManager);

   ///Turn on a stand alone DOMDocumentManager.
   //Kinda a HACK!!!
   void UseStandaloneDOMDocumentManager();

   //Work around for the conflict when exposing the DOMDocumentManager
   void ReadFromFile();

   //Work around for the conflict when exposing the DOMDocumentManager
   void ReadFromString();

   //Work around for the conflict when exposing the DOMDocumentManager
   void WriteToFile();

   //Work around for the conflict when exposing the DOMDocumentManager
   void WriteToString();
   
   ///Get the active DOMDocumentManager.  
   VE_Conductor::DOMDocumentManager* GetDOMDocumentManager();

protected:

   bool _standAloneDDM;///<Tells Reader whether it is using it's on DDM or one was passed in.
   ///Internal function to populate the appropriate structures from the file
   ///read in.
   ///\param rootDocument The document representing the input XML structure.
   virtual void _populateStructureFromDocument(xercesc_2_6::DOMDocument* rootDocument)=0;

   VE_Conductor::DOMDocumentManager* _domDocumentManager;///<The XML document manager.
};
}
#endif// XML_FILE_READER_H
