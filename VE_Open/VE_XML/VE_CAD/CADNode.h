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
 * File:          $RCSfile: CADNode.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
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
   CADNode(XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* rootDocument,std::string name);
   virtual ~CADNode();

   

   ///Set the name of the node in the hierachy.
   ///\param name The name to set for this node.
   void SetNodeName(std::string name);

   ///Set the parent for this node.
   ///\param parent The parent of this node.
   void SetParent(VE_CAD::CADNode* parent);

   ///Set the transform for this node.
   ///\param transform The transform of this node.
   void SetTransform(VE_XML::VETransform* transform);

   ///Set the material for this node.
   ///\param material The material of this node.
   void SetMaterial(VE_CAD::CADMaterial* material);

   ///Set the object from XML data
   ///\param xmlNode Node to set this object from
   virtual void SetObjectFromXMLData( DOMNode* xmlNode);

   ///Get the node type. 
   ///Valid types currently are Node,Assembly,Part and Clone
   std::string GetNodeType();

   ///Get the name of this CAD node.
   std::string GetNodeName();

   ///Get the parent of this CAD node.
   VE_CAD::CADNode* GetParent();

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

   ///Internally update the name of the node in XML.
   void _updateNodeName();

   ///Internally update the type of the node in XML.
   void _updateNodeType();

   VE_XML::VETransform* _transform; ///< Transform for the node.
   VE_CAD::CADMaterial* _material; ///< Material for this node.
   VE_CAD::CADNode* _parent;  ///< Parent node.
   std::string _name;///< The name of this node.
   std::string _type;///< The type of node;
};
}
#endif// _CAD_NODE_H_
