#ifndef _CAD_ASSEMBLY_H_
#define _CAD_ASSEMBLY_H_
#include <xercesc/dom/DOM.hpp>
#include "VE_Installer/include/VEConfig.h"
#include <VE_Open/VE_XML/VE_CAD/CADNode.h>
#include <string>
#include <vector>
/*!\file CADAssembly.h
 * CADAssembly API
 */

/*! \class VE_CAD::CADAssembly
 * Class to represent a CAD assembly.
 * It's children can be CADAssemblies, CADParts or CADClones.
 * There isn't an actual geometry that is represented here but instead
 * this is more of an organization node.
 */
XERCES_CPP_NAMESPACE_USE
namespace VE_CAD{
class VE_CAD_EXPORTS CADAssembly: public VE_CAD::CADNode{
public:
   ///Constructor
   ///\param rootDocument The root XML document of this assembly.
   ///\param name Name of the assembly.
   CADAssembly(DOMDocument* rootDocument,std::string name=std::string("Assembly"));
   virtual ~CADAssembly();

   ///Add a child to this assembly
   void AddChild(VE_CAD::CADNode* node);

   ///Set the object from XML data
   ///\param xmlNode Node to set this object from
   virtual void SetObjectFromXMLData( DOMNode* xmlNode);

   ///\param node The pointer of the node to remove from this assembly
   ///Remove child from the assembly returns true for success false if fails
   bool RemoveChild(VE_CAD::CADNode* node);

   ///\param whichChild The index of the node to remove from this assembly
   ///Remove child from the assembly returns true for success false if fails
   bool RemoveChild(unsigned int whichChild); 

   ///Get the number of children of this assembly
   unsigned int GetNumberOfChildren();

   ///Get a specified child of this assembly
   VE_CAD::CADNode* GetChild(unsigned int whichChild);
   
   ///Copy constructor
   CADAssembly(const CADAssembly& rhs);

   ///Equal operator
   CADAssembly& operator=(const CADAssembly& rhs);
protected:
   

   ///Internally update the XML data for this node.
   ///\param input The XML element data.
   virtual void _updateVEElement(std::string input);
   unsigned int _nChildren; ///<number of children in this assembly
   ///\typedef ChildList
   /// A vector of CADNodes
   typedef std::vector<VE_CAD::CADNode*> ChildList; 
   ChildList _children; ///<a list of the children
};
}
#endif// _CAD_ASSEMBLY_H_
