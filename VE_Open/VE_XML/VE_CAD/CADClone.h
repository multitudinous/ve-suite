#ifndef _CAD_CLONE_H_
#define _CAD_CLONE_H_
#include <xercesc/dom/DOM.hpp>
#include "VE_Installer/include/VEConfig.h"
#include <VE_Open/VE_XML/VE_CAD/CADNode.h>
#include <string>
/*!\file CADClone.h
  CADNode API
  */
/*!\class VE_CAD::CADClone
 * This class is the base class for representing
 * the hierarchy of a CAD structure.
 */
XERCES_CPP_NAMESPACE_USE
namespace VE_CAD{
class VE_CAD_EXPORTS CADClone:public VE_CAD::CADNode{
public:
   ///Constructor
   ///\param rootDocument The root XML document for this node.
   ///\param name The name of this node.
   ///\param originalNode The node we are going to clone.
   CADClone(DOMDocument* rootDocument,std::string name=std::string("Clone"),VE_CAD::CADNode* originalNode=0);
   virtual ~CADClone();

   ///Set the object from XML data
   ///\param xmlNode Node to set this object from
   virtual void SetObjectFromXMLData( DOMNode* xmlNode);
 
   ///Copy constructor
   CADClone(const CADClone& rhs);

   ///Equal operator
   CADClone& operator=(const CADClone& rhs);
protected:
  

   ///Internally update the XML data for this node;
   ///\param input The new XML data for this element;
   virtual void _updateVEElement(std::string input);
   VE_CAD::CADNode* _originalNode;///< The node that is instanced
};
}
#endif// _CAD_CLONE_H_
