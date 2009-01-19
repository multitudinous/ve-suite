/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef _CAD_NODE_H_
#define _CAD_NODE_H_

#include <ves/open/xml/cad/CADNodePtr.h>
#include <ves/open/xml/TransformPtr.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/cad/CADAttributePtr.h>
#include <ves/open/xml/cad/CADNodeAnimationPtr.h>
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

namespace ves
{
namespace open
{
namespace xml
{
namespace cad
{
class VE_CAD_EXPORTS CADNode: public ves::open::xml::XMLObject
{
public:
    virtual ~CADNode();

    ///Set the name of the node in the hierachy.
    ///\param name The name to set for this node.
    void SetNodeName( const std::string& name );

    ///Set the parent for this node.
    ///\param parentID The parent ID of this node.
    void SetParent( const std::string& parentID );

    ///Set the transform for this node.
    ///\param transform The transform of this node.
    void SetTransform( ves::open::xml::TransformPtr transform );

    ///Add an attribute for this node.
    ///\param attribute A new attribute for this node.
    void AddAttribute( ves::open::xml::cad::CADAttributePtr attribute );

    ///Add an animation file for this CADNode.
    ///\param animationFileName The path to the animation file.
    ///\param animationReferenceName The reference name to the CADNodeAnimation.
    void AddAnimation( const std::string& animationReferenceName,
                       const std::string& animationFileName );

    ///Remove an attribute from the node
    ///\param attributeName The name of the attribute to remove.
    void RemoveAttribute( const std::string& attributeName );

    ///Set the name of the active attribute
    ///\param attributeName The name of the active attribute.
    void SetActiveAttribute( const std::string& attributeName );

    ///Toggle this node "ON" or "OFF"
    ///\param onOff Flag for visibility\n true == "ON"\n false== "OFF"
    void SetVisibility( bool onOff );

    ///Set the opacity of this node
    ///\param alpha The alpha value
    void SetOpacity( float alpha );

    ///Set the object from XML data
    ///\param xmlNode Node to set this object from
    virtual void SetObjectFromXMLData( XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlNode );

    ///Determine if the node is visible or not
    bool GetVisibility();

    ///Get the active attribute
    ves::open::xml::cad::CADAttributePtr GetActiveAttribute();

    ///Get the node type.
    ///Valid types currently are Node,Assembly,Part and Clone
    std::string GetNodeType();

    ///Get the name of this CAD node.
    std::string GetNodeName();

    ///Get the parent of this CAD node.
    std::string GetParent();

    ///Check if this CADNode has any animation information.
    bool HasAnimation();

    ///Check if this CADNode has physics enabled.
    bool HasPhysics();

    ///Enable physics for the CADNode.
    void EnablePhysics();

    ///Set the mass of this CAD node.
    ///\return The mass of the node
    void SetMass( double mass );

    ///Get the mass of this CAD node.
    ///\return The mass of the node
    double GetMass();

    ///Set the friction of this CAD node.
    ///\param friction The friction coeffecient
    void SetFriction( double friction );

    ///Get the friction of this CAD node.
    ///\param The friction coeffecient
    double GetFriction();

    ///Set the restitution of this CAD node.
    ///\param restitution The restitution value
    void SetRestitution( double restitution );

    ///Get the restitution of this CAD node.
    ///\return The restitution value
    double GetRestitution();

    ///Set the physics motion type can be static or dynamic
    ///\param physicsMotionType The motion type container
    void SetPhysicsMotionType( const std::string& physicsMotionType );
    ///Get the physics motion type
    ///\return The motion type
    const std::string& GetPhysicsMotionType();
    ///\Get the LOD type can be overal or compound
    ///\param physicsLODType The LOD container
    void SetPhysicsLODType( const std::string& physicsLODType );
    ///Get the LOD type for the physics mesh
    ///\return The LOD type string
    const std::string& GetPhysicsLODType();
    ///Get the physics mesh type can be box, cylinder, sphere, or mesh
    ///\param physicsMeshType The mesh type
    void SetPhysicsMeshType( const std::string& physicsMeshType );
    ///Get the physics mesh type
    ///\return The physics mesh type container
    const std::string& GetPhysicsMeshType();
    
    ///Set the physics mesh of this CAD node.
    //void SetPhysicsMesh( const std::string& physicsMesh );

    ///Get the physics mesh of this CAD node.
    //std::string GetPhysicsMesh();

    ///Get the transform of this CAD node.
    ves::open::xml::TransformPtr GetTransform();

    ///Get an attribute of this CAD node by a name
    ///\param name The name of the attribute to find.
    ves::open::xml::cad::CADAttributePtr GetAttribute( const std::string& name );

    ///Get an attribute of this CAD node by a name
    ///\param index The index of the attribute to find.
    ves::open::xml::cad::CADAttributePtr GetAttribute( unsigned int index );

    ///There is probably only one of these but internally we keep a vector of them
    ///\param name The reference name of the animation to find.
    ves::open::xml::cad::CADNodeAnimationPtr GetAnimation( const std::string& name );

    ///There is probably only one of these but internally we keep a vector of them
    ///\param index The animation file index in the list.
    ves::open::xml::cad::CADNodeAnimationPtr GetAnimation( unsigned int index );

    ///Get the CADNodeAnimation for this CADNode.
    size_t GetNumberOfAnimations();

    ///Get attributes for this node.
    std::vector<CADAttributePtr> GetAttributeList();

    ///Get the opacity attribute of this node
    float GetOpacity();
    ///Get the mode of how to treat this node when vis is selected
    bool GetTransparentFlag();
    ///Set the mode of how to treat this node when vis is selected
    void SetTransparentFlag( bool transparent );
    
protected:
    ///Constructor
    ///\param name The name of this node.
    CADNode( const std::string& name );

    ///Copy constructor
    ///\param rhs The CADNode to copy
    ///\param clone Create a clone of this node
    CADNode( const CADNode& rhs, bool clone = false );
    
    ///Equal operator
    CADNode& operator=( const CADNode& rhs );
        
    ///Internally update the XML data for this element.
    ///\param input The XML element information
    virtual void _updateVEElement( const std::string& input );

    ///Internally update the name of the node in XML.
    void _updateNodeName();

    ///Internally update the type of the node in XML.
    void _updateNodeType();

    ///The name of the active attribute.
    std::string m_activeAttributeName;
    ///Parent node ID.
    std::string  m_parent;
    ///Transform for the node.
    ves::open::xml::TransformPtr m_transform;
    ///A list of attributes for this node
    std::vector<ves::open::xml::cad::CADAttributePtr> m_attributeList;
    ///A list of animation path files for this node.
    std::vector<ves::open::xml::cad::CADNodeAnimationPtr> m_animations;
    ///The name of this node.
    std::string m_name;
    ///The type of node;
    std::string m_type;
    ///Node visibilty.
    bool m_visibility;

    ///Physics variables
    ///Node physics.
    bool m_physics;
    ///Node mass.
    double m_mass;
    ///Node friction.
    double m_friction;
    ///Node restitution.
    double m_restitution;
    ///Physics Mesh type
    std::string mPhysicsMeshType;
    ///Physics LOD type
    std::string mPhysicsLODType;
    ///Physics Motion Type
    std::string mPhysicsMotionType;
    ///Node physics mesh.
    std::string mPhysicsMesh;
    ///Opacity value for this node
    float mOpacity;
    ///Tells xplorer whether or not to change this node transparent when
    ///vis is selected
    bool mMakeTransparentOnVis;
};

}
}
}
}
#endif// _CAD_NODE_H_
