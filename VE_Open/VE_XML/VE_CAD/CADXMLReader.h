#ifndef CAD_XML_FILE_READER_H
#define CAD_XML_FILE_READER_H

/*!\file CADXMLReader.h
  CADXMLReader API
  */

/*!\class VE_CAD::CADXMLReader
 * Class to read in a CAD XML file.
 */
#include "VE_Open/VE_XML/XMLReader.h"
#include "VE_Installer/include/VEConfig.h"
namespace VE_CAD
{
   class CADNode;
}
XERCES_CPP_NAMESPACE_USE
namespace VE_CAD{
   class VE_CAD_EXPORTS CADXMLReader: public VE_XML::XMLReader{
public:
   ///Default Constructor
   CADXMLReader();
   
   ///Copy Constructor
   CADXMLReader(const CADXMLReader& fr);

   ///Destructor
   virtual ~CADXMLReader();

   ///Get the root node of the CAD Hierarchy
   VE_CAD::CADNode* GetRootNode();

   ///Equal Operator
   CADXMLReader& operator=(const CADXMLReader& rhs);
protected:
   ///Internal function to populate the appropriate structures from the file
   ///read in.
   ///\param rootDocument The document representing the input XML structure.
   virtual void _populateStructureFromDocument(XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* rootDocument);
   VE_CAD::CADNode* _rootNode;///< The rootNode of the XML Data read in.
};
}
#endif// CAD_XML_READER_H
