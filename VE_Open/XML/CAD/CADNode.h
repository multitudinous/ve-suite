/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _CAD_NODE_H_
#define _CAD_NODE_H_

#include "VE_Open/XML/XMLObject.h"
#include "VE_Open/XML/CAD/CADAttribute.h"
#include <xercesc/dom/DOM.hpp>
#include <string>
#include <vector>

/*!\file CADNode.h
  CADNode API
  */
/*!\class VE_XML::VE_CAD::CADNode
 * This class is the base class for representing
 * the hierarchy of a CAD structure.
 */

/*!\namespace VE_CAD
 * Contains nodes for creating/managing a CAD hierarchy.
 */

namespace VE_XML
{
   class Transform;
}

namespace VE_XML
{
namespace VE_CAD
{
class CADAssembly;
class CADNodeAnimation;

class VE_CAD_EXPORTS CADNode: public VE_XML::XMLObject
{
public:
   ///Constructor
   ///\param name The name of this node.
   CADNode(std::string name);
   virtual ~CADNode();

   ///Set the name of the node in the hierachy.
   ///\param name The name to set for this node.
   void SetNodeName(std::string name);

   ///Set the parent for this node.
   ///\param parentID The parent ID of this node.
   //void SetParent(unsigned int parentID);

   ///Set the parent for this node.
   ///\param parentID The parent ID of this node.
   void SetParent(std::string parentID);

   ///Set the transform for this node.
   ///\param transform The transform of this node.
   void SetTransform(VE_XML::Transform* transform);

   ///Add an attribute for this node.
   ///\param attribute A new attribute for this node.
   void AddAttribute(VE_CAD::CADAttribute attribute);

   ///Add an animation file for this CADNode.
   ///\param animationFileName The path to the animation file.
   ///\param animationReferenceName The reference name to the CADNodeAnimation.
   void AddAnimation(std::string animationReferenceName,
                     std::string animationFileName);

   ///Remove an attribute from the node
   ///\param attributeName The name of the attribute to remove.
   void RemoveAttribute(std::string attributeName);

   ///Set the name of the active attribute
   ///\param attributeName The name of the active attribute.
   void SetActiveAttribute(std::string attributeName); 

   ///Toggle this node "ON" or "OFF"
   ///\param onOff Flag for visibility\n true == "ON"\n false== "OFF"
   void SetVisibility(bool onOff);

   ///Set the object from XML data
   ///\param xmlNode Node to set this object from
   virtual void SetObjectFromXMLData( XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlNode);

   ///Determine if the node is visible or not
   bool GetVisibility();
 
   ///Get the active attribute
   VE_XML::VE_CAD::CADAttribute& GetActiveAttribute();

   ///Get the node type. 
   ///Valid types currently are Node,Assembly,Part and Clone
   std::string GetNodeType();

   ///Get the name of this CAD node.
   std::string GetNodeName();

   ///Get the parent of this CAD node.
   //unsigned int GetParent();

   ///Get the parent of this CAD node.
   std::string GetParent();

   ///Check if this CADNode has any animation information.
   bool HasAnimation();
   
   ///Check if this CADNode has physics enabled.
   bool HasPhysics();

   ///Enable physics for the CADNode.
   void EnablePhysics();

   ///Get the transform of this CAD node.
   VE_XML::Transform* GetTransform();

   ///Get an attribute of this CAD node by a name
   ///\param name The name of the attribute to find.
  VE_XML::VE_CAD::CADAttribute& GetAttribute(std::string name);
  
    ///Get an attribute of this CAD node by a name
   ///\param index The index of the attribute to find.
  VE_XML::VE_CAD::CADAttribute& GetAttribute(unsigned int index);

   ///There is probably only one of these but internally we keep a vector of them
   ///\param name The reference name of the animation to find.
  VE_XML::VE_CAD::CADNodeAnimation& GetAnimation(std::string name);

   ///There is probably only one of these but internally we keep a vector of them
   ///\param index The animation file index in the list.
  VE_XML::VE_CAD::CADNodeAnimation& GetAnimation(unsigned int index);

   ///Get the CADNodeAnimation for this CADNode.
   size_t GetNumberOfAnimations();

   ///Get attributes for this node.
   std::vector<CADAttribute> GetAttributeList();

   ///Copy constructor
   ///\param rhs The CADNode to copy
   ///\param clone Create a clone of this node
   CADNode(const CADNode& rhs, bool clone=false);

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

   std::string m_activeAttributeName;///<The name of the active attribute.
   //unsigned int _uID;///<A "unique" id for the node.
   std::string  m_parent;  ///< Parent node ID.
   VE_XML::Transform* m_transform; ///< Transform for the node.
   std::vector<VE_CAD::CADAttribute> m_attributeList;///<A list of attributes for this node
   std::vector<VE_CAD::CADNodeAnimation> m_animations;//<A list of animation path files for this node.
   std::string m_name;///< The name of this node.
   std::string m_type;///< The type of node;
   bool m_visibility;///<Node visibilty.
   bool m_physics;///<Node physics.
};
}
template<>
inline XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* XMLObject::SetSubElement(const std::string subElementTagName, VE_CAD::CADNode* val)
{
   val->SetOwnerDocument( _rootDocument );
   XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* childElement = val->GetXMLData( subElementTagName );
   _veElement->appendChild( childElement );
   return childElement;
}
}
#endif// _CAD_NODE_H_
