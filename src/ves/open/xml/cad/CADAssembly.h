/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
#ifndef _CAD_ASSEMBLY_H_
#define _CAD_ASSEMBLY_H_
#include <xercesc/dom/DOM.hpp>
#include <ves/open/xml/cad/CADNode.h>
#include <string>
#include <vector>
/*!\file CADAssembly.h
 * CADAssembly API
 */

/*! \class VE_XML::VE_CAD::CADAssembly
 * Class to represent a CAD assembly.
 * It's children can be CADAssemblies, CADParts.
 * There isn't an actual geometry that is represented here but instead
 * this is more of an organization node.
 */
XERCES_CPP_NAMESPACE_USE

namespace ves
{
namespace open
{
namespace xml
{
namespace cad
{
class VE_CAD_EXPORTS CADAssembly: public ves::open::xml::cad::CADNode
{
public:
    ///Constructor
    ///\param name Name of the assembly.
    CADAssembly( std::string name = std::string( "Assembly" ) );
    virtual ~CADAssembly();

    ///Add a child to this assembly
    ///\param node The node to add to the assembly
    //void AddChild(VE_XML::VE_CAD::CADNode node);

    ///Add a child to this assembly
    ///\param node The node to add to the assembly
    void AddChild( ves::open::xml::cad::CADNode* node );

    ///Set the object from XML data
    ///\param xmlNode Node to set this object from
    virtual void SetObjectFromXMLData( DOMNode* xmlNode );

    ///Set the associated dataset (a ParameterBlock) to this CADNode
    ///\param parameterBlockUUID The unique identifer to the dataset ParameterBlock
    void SetAssociatedDataset( std::string parameterBlockUUID );

    ///Get the associated dataset
    ///\param parameterBlockUUID The unique identifer to the dataset ParameterBlock if it exists
    ///\return True/False if there is an associated dataset
    bool GetAssociatedDataset( std::string& parameterBlockUUID );

    ///\param node The pointer of the node to remove from this assembly
    ///Remove child from the assembly returns true for success false if fails
    ///\todo This function is NOT implemented yet and will ALWAYS return false!!!
    bool RemoveChild( ves::open::xml::cad::CADNode node );

    ///\param whichChildID The ID of the node to remove from this assembly
    ///Remove child from the assembly returns true for success false if fails
    bool RemoveChild( std::string whichChildID );

    ///Get the number of children of this assembly
    unsigned int GetNumberOfChildren();

    ///Get a specified child of this assembly
    ves::open::xml::cad::CADNode* GetChild( unsigned int whichChild );

    ///Get a child by a name
    ///\param name The of the child name to search for.
    ves::open::xml::cad::CADNode* GetChild( std::string name );

    ///Copy constructor
    //\param rhs The CADPart to copy
    ///\param clone Create a clone of this node
    CADAssembly( const CADAssembly& rhs, bool clone = false );

    ///Equal operator
    CADAssembly& operator=( const CADAssembly& rhs );
protected:


    ///Internally update the XML data for this node.
    ///\param input The XML element data.
    virtual void _updateVEElement( std::string input );

    ///Update the child nodes of this assembly
    void _updateChildren();

    unsigned int m_numChildren; ///<number of children in this assembly
    ///\typedef ChildList
    /// A vector of VE_XML::VE_CAD::CADNode s
    typedef std::vector<ves::open::xml::cad::CADNode*> ChildList;
    ChildList m_children; ///<A list of the children
    std::string m_associatedDataset;///<The dataset associated with this CADNode
};
}
template<>
inline XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* XMLObject::SetSubElement( const std::string subElementTagName, ves::open::xml::cad::CADAssembly* val )
{
    val->SetOwnerDocument( _rootDocument );
    XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* childElement = val->GetXMLData( subElementTagName );
    _veElement->appendChild( childElement );
    return childElement;
}
}
}
}
#endif// _CAD_ASSEMBLY_H_
