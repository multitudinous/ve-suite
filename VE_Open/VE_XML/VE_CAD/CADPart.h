#ifndef _CAD_PART_H_
#define _CAD_PART_H_
/*!\file CADPart.h
 * CADPart API
 */

/*!\class VE_CAD::CADPart 
 * Class to represent a part file (the actual CAD geometry)
 */
#include <xercesc/dom/DOM.hpp>
#include "VE_Installer/include/VEConfig.h"
#include "VE_Open/VE_XML/VE_CAD/CADNode.h"
#include <string>

XERCES_CPP_NAMESPACE_USE
namespace VE_CAD{
class VE_CAD_EXPORTS CADPart: public VE_CAD::CADNode{
public:
   ///Constructor
   ///\param rootDocument The root XML document 
   ///\param name The name of the part
   CADPart(DOMDocument* rootDocument,std::string name=std::string("Part"));

   ///Destructor
   virtual ~CADPart();

   ///\param cadFileName The name of the part
   ///Set the name of the CAD file this node represents
   void SetCADFileName(std::string cadFileName);

   ///Set the object from XML data
   ///\param xmlNode Node to set this object from
   virtual void SetObjectFromXMLData( DOMNode* xmlNode);

   ///Get the name of the CAD file this node represents
   std::string GetCADFileName();

   ///Copy constructor
   CADPart(const CADPart& rhs);

   ///Equal operator
   CADPart& operator=(const CADPart& rhs);
protected:
   

   ///Internally update the XML data for this node.
   ///\param input The XML data for this element.
   virtual void _updateVEElement(std::string input);

   ///Internally update the XML data for the CAD filename that this part represents.
   void _updateCADFileName();
   
   std::string _cadFileName; ///<The name of the CAD file on disk
};
}
#endif// _CAD_PART_H_
