#ifndef _CAD_CLONE_H_
#define _CAD_CLONE_H_
#include <xercesc/dom/DOM.hpp>
#include "VE_Installer/include/VEConfig.h"
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
   CADClone(DOMDocument* rootDocument,std::string name,VE_CAD::CADNode* originalNode);
   virtual ~CADClone();

protected:
   ///Internally update the XML data for this node;
   virtual void _updateVEElement();
   VE_CAD::CADNode* _originalNode;///< The node that is instanced
};
}
#endif// _CAD_CLONE_H_
