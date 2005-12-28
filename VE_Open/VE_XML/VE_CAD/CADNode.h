#ifndef _CAD_NODE_H_
#define _CAD_NODE_H_

#include "VE_Open/VE_XML/VEXMLObject.h"
#include "VE_Installer/include/VEConfig.h"
#include <xercesc/dom/DOM.hpp>
#include <string>

/*!\file CADNode.h
  CADNode API
  */
/*!\class VE_CAD::CADNode
 * This class is the base class for representing
 * the hierarchy of a CAD structure.
 */

/*!\namespace VE_CAD
 * Contains nodes for creating/managing a CAD hierarchy.
 */

namespace VE_XML{
   class VETransform;
}
XERCES_CPP_NAMESPACE_USE

namespace VE_CAD{

class CADAssembly;
class CADMaterial;

class VE_CAD_EXPORTS CADNode: public VE_XML::VEXMLObject{
public:
   ///Constructor
   ///\param rootDocument The xerces document for this node.
   ///\param name The name of this node.
   CADNode(DOMDocument* rootDocument,std::string name);
   virtual ~CADNode();

   ///Set the name of the node in the hierachy.
   ///\param name The name to set for this node.
   void SetNodeName(std::string name);

   ///Set the parent for this node.
   ///\param parent The parent of this node.
   void SetParent(VE_CAD::CADAssembly* parent);

   ///Set the transform for this node.
   ///\param transform The transform of this node.
   void SetTransform(VE_XML::VETransform* transform);

   ///Set the material for this node.
   ///\param material The material of this node.
   void SetMaterial(VE_CAD::CADMaterial* material);

   ///Set the object from XML data
   ///\param xmlNode Node to set this object from
   virtual void SetObjectFromXMLData( DOMNode* xmlNode);

   ///Get the name of this CAD node.
   std::string GetNodeName();

   ///Get the parent of this CAD node.
   VE_CAD::CADAssembly* GetParent();

   ///Get the transform of this CAD node.
   VE_XML::VETransform* GetTransform();

   ///Get the material of this CAD node
   VE_CAD::CADMaterial* GetMaterial();
   
   ///Copy constructor
   CADNode(const CADNode& rhs);

   ///Equal operator
   CADNode& operator=(const CADNode& rhs);

protected:
   
   ///Internally update the XML data for this element.
   ///\param input The XML element information
   virtual void _updateVEElement(std::string input);
   VE_XML::VETransform* _transform; ///< Transform for the node.
   VE_CAD::CADMaterial* _material; ///< Material for this node.
   VE_CAD::CADAssembly* _parent;  ///< Parent node.
   std::string _name;///< The name of this node.
};
}
#endif// _CAD_NODE_H_
