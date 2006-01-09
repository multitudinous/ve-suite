/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * File:          $RCSfile: CADAssembly.h,v $
 * Date modified: $Date: 2005-07-11 13:47:16 -0500 (Mon, 11 Jul 2005) $
 * Version:       $Rev: 2653 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
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
   ///\todo This function is NOT implemented yet and will ALWAYS return false!!!
   bool RemoveChild(VE_CAD::CADNode* node);

   ///\param whichChild The index of the node to remove from this assembly
   ///Remove child from the assembly returns true for success false if fails 
   ///\todo This function is NOT implemented yet and will ALWAYS return false!!!
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

   ///Update the child nodes of this assembly
   void _updateChildren();
  
   unsigned int _numChildren; ///<number of children in this assembly
   ///\typedef ChildList
   /// A vector of VE_CAD::CADNode s
   typedef std::vector<VE_CAD::CADNode*> ChildList; 
   ChildList _children; ///<A list of the children
};
}
#endif// _CAD_ASSEMBLY_H_
