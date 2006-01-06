#ifndef XML_READER_WRITER_H
#define XML_READER_WRITER_H

/*!\file XMLReaderWriter.h
  XMLReaderWriter API
  */

/*!\class XMLReaderWriter
 *Abstract class to set things up for reading/writing VE-Suite XML files .
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
class VE_XML_EXPORTS XMLReaderWriter{
public:
   ///Default Constructor
   XMLReaderWriter();
   
   ///Destructor
   virtual ~XMLReaderWriter();

   ///Read an input XML string of type.
   ///\param xmlData The input XML type.
   virtual void ReadXMLData(std::string xmlData);

   ///Write the current XML document
   ///\param xmlData The XML document to write to.
   virtual void WriteXMLDocument(std::string xmlData);

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
   virtual void _populateStructureFromDocument(XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* rootDocument)=0;

   VE_Conductor::DOMDocumentManager* _domDocumentManager;///<The XML document manager.
};
}
#endif// XML_READER_WRITER_H
