#ifndef _CAD_CLONE_H_
#define _CAD_CLONE_H_
#include "VE_Installer/include/VEConfig.h"
/*!\file CADClone.h
  CADNode API
  */
/*!\class VE_CAD::CADClone
 * This class is the base class for representing
 * the hierarchy of a CAD structure.
 */

namespace VE_CAD{
class VE_CAD_EXPORTS CADClone:public VE_CAD::CADNode{
public:
   ///Constructor
   CADClone(std::string name,VE_CAD::CADNode* originalNode);
   virtual ~CADClone();

protected:
   ///Internally update the XML data for this node;
   virtual void _updateVEElement();
   VE_CAD::CADNode* _originalNode;///< The node that is instanced
};
}
#endif// _CAD_CLONE_H_
