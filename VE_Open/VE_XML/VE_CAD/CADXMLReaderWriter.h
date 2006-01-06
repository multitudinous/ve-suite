#ifndef CAD_XML_FILE_READER_H
#define CAD_XML_FILE_READER_H

/*!\file CADXMLReaderWriter.h
  CADXMLReaderWriter API
  */

/*!\class VE_CAD::CADXMLReaderWriter
 * Class to read in a CAD XML file.
 */
#include "VE_Open/VE_XML/XMLReaderWriter.h"
#include "VE_Installer/include/VEConfig.h"
namespace VE_CAD
{
   class CADNode;
}
XERCES_CPP_NAMESPACE_USE
namespace VE_CAD{
   class VE_CAD_EXPORTS CADXMLReaderWriter: public VE_XML::XMLReaderWriter{
public:
   ///Default Constructor
   CADXMLReaderWriter();
   
   ///Copy Constructor
   CADXMLReaderWriter(const CADXMLReaderWriter& fr);

   ///Destructor
   virtual ~CADXMLReaderWriter();

   ///Get the root node of the CAD Hierarchy
   VE_CAD::CADNode* GetRootNode();

   ///Equal Operator
   CADXMLReaderWriter& operator=(const CADXMLReaderWriter& rhs);
protected:
   ///Internal function to populate the appropriate structures from the file
   ///read in.
   ///\param rootDocument The document representing the input XML structure.
   virtual void _populateStructureFromDocument(XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* rootDocument);
   VE_CAD::CADNode* _rootNode;///< The rootNode of the XML Data read in.
};
}
#endif// CAD_XML_READER_H
