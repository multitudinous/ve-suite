#ifndef _CAD_NODE_H_
#define _CAD_NODE_H_

#include <VE_XML/VEXMLObject.h>
#include <string>
#include "VE_Installer/include/VEConfig.h"
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
namespace VE_CAD{
class CADAssembly;
class CADMaterial;

class VE_CAD_EXPORTS CADNode: public VEXMLObject{
public:
   ///Constructor
   CADNode(std::string name=std::string("CADGeometry"));
   virtual ~CADNode();

   ///Set the name of the node in the hierachy.
   ///\param name The name to set for this node.
   void SetNodeName(std::string name);

   ///Set the parent for this node.
   ///\param parent The parent of this node.
   void SetParent(VE_CAD::CADAssembly* parent);

   ///Set the transform for this node.
   ///\param transform The transform of this node.
   void SetParent(VE_XML::VETransform* transform);

   ///Set the material for this node.
   ///\param material The material of this node.
   void SetMaterial(VE_CAD::CADMaterial* material);

   ///Get the name of this CAD node.
   std::string GetNodeName();

   ///Get the parent of this CAD node.
   VE_CAD::CADAssembly* GetParent();

   ///Get the transform of this CAD node.
   VE_XML::Transform* GetTransform();

   ///Get the material of this CAD node
   VE_CAD::CADMaterial* GetMaterial();
protected:
   ///Internally update the XML data for this element.
   virtual void _updateVEElement();
   VE_XML::VETransform* _transform; ///< Transform for the node.
   VE_CAD::CADMaterial* _material; ///< Material for this node.
   VE_CAD::CADAssembly* _parent;  ///< Parent node.
};
}
#endif// _CAD_NODE_H_
