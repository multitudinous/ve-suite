/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
#include <ves/open/xml/cad/CADAssembly.h>
#include <ves/open/xml/cad/CADPart.h>
#include <ves/open/xml/XMLObjectFactory.h>
#include <sstream>
XERCES_CPP_NAMESPACE_USE

using namespace ves::open::xml::cad;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////
CADAssembly::CADAssembly( const std::string& name )
        : ves::open::xml::cad::CADNode( name )
{

    mAssociatedDataset = "NONE";
    m_type = std::string( "Assembly" );
    SetObjectType( "CADAssembly" );
}
///////////////////////////
///Destructor            //
///////////////////////////
CADAssembly::~CADAssembly()
{
    mChildren.clear();
}
/////////////////////////////////////////////////
void CADAssembly::AddChild( ves::open::xml::cad::CADNodePtr node )
{
    mChildren.push_back( node );
    mChildren.back()->SetParent( mUuid );
}
//////////////////////////////////////////////////////////////////////
void CADAssembly::SetAssociatedDataset( const std::string& parameterBlockmUuid )
{
    mAssociatedDataset = parameterBlockmUuid;
}
///////////////////////////////////////////////////////////////////////
bool CADAssembly::GetAssociatedDataset( std::string& parameterBlockmUuid )
{
    if( mAssociatedDataset != "NONE" )
    {
        parameterBlockmUuid = mAssociatedDataset;
        return true;
    }
    return false;
}
////////////////////////////////////////////////////
bool CADAssembly::RemoveChild( ves::open::xml::cad::CADNode node )
{
    RemoveChild( node.GetID() );
    return false;
}
//////////////////////////////////////////////////////
bool CADAssembly::RemoveChild( const std::string& whichChildID )
{
    std::vector<CADNodePtr>::iterator childToRemove;
    for( childToRemove = mChildren.begin();
            childToRemove != mChildren.end();
            childToRemove++ )
    {
        CADNodePtr childNode = ( *childToRemove );
        if( whichChildID  == childNode->GetID() )
        {
            mChildren.erase( childToRemove );
            return true;
        }
    }
    return false;
}
///////////////////////////////////////////////
unsigned int CADAssembly::GetNumberOfChildren()
{
    return mChildren.size();
}
/////////////////////////////////////////////////////////
ves::open::xml::cad::CADNodePtr CADAssembly::GetChild( const std::string& name )
{
    for( size_t i = 0; i < mChildren.size(); i++ )
    {
        if( mChildren.at( i )->GetNodeName() == name )
        {
            return mChildren.at( i );
        }
    }
    return ves::open::xml::cad::CADNodePtr();
}
///////////////////////////////////////////////////////////////
ves::open::xml::cad::CADNodePtr CADAssembly::GetChild( unsigned int whichChild )
{
    return mChildren.at( whichChild );
}
///////////////////////////////////
void CADAssembly::_updateChildren()
{
    DOMElement* childList = mRootDocument->createElement(
                            Convert( "children" ).toXMLString() );

    //the number of children
    /*DOMElement* nchildrenElement = mRootDocument->createElement(
                                   Convert( "numChildren" ).toXMLString() );

    std::stringstream int2string;
    int2string << mChildren.size();
    DOMText* numberOfChildren = mRootDocument->createTextNode(
                                Convert( int2string.str() ).toXMLString() );

    nchildrenElement->appendChild( numberOfChildren );
    mVeElement->appendChild( nchildrenElement );*/

    //SetSubElement( "numChildren", mChildren.size() );
    //add the children nodes to the list
    for( unsigned int i = 0; i < mChildren.size();  i++ )
    {
        mChildren.at( i )->SetOwnerDocument( mRootDocument );
        mChildren.at( i )->SetParent( mUuid );
        childList->appendChild( mChildren.at( i )->GetXMLData( "child" ) );
    }
    mVeElement->appendChild( childList );
}
/////////////////////////////////////////////////////
void CADAssembly::_updateVEElement( const std::string& input )
{
    //Get the base elements from CADNode
    ves::open::xml::cad::CADNode::_updateVEElement( input );
    _updateChildren();
    SetAttribute( "associatedDataset", mAssociatedDataset );
}
/////////////////////////////////////////////////////
void CADAssembly::SetObjectFromXMLData( DOMNode* xmlNode )
{
    DOMElement* currentElement = 0;

    if( xmlNode->getNodeType() == DOMNode::ELEMENT_NODE )
    {
        currentElement = dynamic_cast<DOMElement*>( xmlNode );
    }

    if( !currentElement )
    {
        return;
    }
    //populate the base elements in node
    ves::open::xml::cad::CADNode::SetObjectFromXMLData( xmlNode );

    //clear out the current list of children
    mChildren.clear();

    //get the new number of children
    {
        //DOMElement* nChildrenElement = GetSubElement( currentElement, std::string( "numChildren" ), 0 );
        //XMLObject::GetAttribute( nChildrenElement, "numChildren", mChildren.size() );
    }
    if( currentElement->getAttributeNode(
        Convert( "associatedDataset" ).toXMLString() ) )
    {
        XMLObject::GetAttribute( currentElement,
                "associatedDataset",
                mAssociatedDataset );
    }
    else
    {
        mAssociatedDataset = "NONE";
    }
    //populate the childList
    {
        DOMNodeList* childList = currentElement->getElementsByTagName(
                                 Convert( "children" ).toXMLString() );

        DOMElement* childListElement = dynamic_cast<DOMElement*>( childList->item( 0 ) );
        DOMNodeList* childrenNodes = childListElement->getElementsByTagName(
                                     Convert( "child" ).toXMLString() );

        size_t nChilderenReally = childrenNodes->getLength();
        for( size_t i = 0; i < nChilderenReally; i++ )
        {
            DOMElement* cadNode = dynamic_cast<DOMElement*>( childrenNodes->item( i ) );
            unsigned int g = 0;
            while( cadNode->getParentNode() != childListElement )
            {
                i++;
                cadNode = dynamic_cast<DOMElement*>( childrenNodes->item( i ) );
                if( i == nChilderenReally )
                    return;
            }
            DOMElement* nodeType = 0;
            unsigned int k = 0;
            while( !nodeType )
            {
                nodeType = GetSubElement( cadNode, std::string( "type" ), k );
                k++;
            }
            if( nodeType )
            {
                std::string tmpNodeType;
                GetDataFromElement( nodeType, tmpNodeType );
                if( tmpNodeType == std::string( "Assembly" ) )
                {
                    //this is an Assembly
                    ves::open::xml::cad::CADAssemblyPtr newAssembly( new ves::open::xml::cad::CADAssembly() );
                    //VE_XML::VE_CAD::CADAssembly newAssembly;// = new VE_XML::VE_CAD::CADAssembly();
                    newAssembly->SetObjectFromXMLData( cadNode );
                    newAssembly->SetParent( mUuid );
                    mChildren.push_back( newAssembly );
                }
                else if( tmpNodeType == std::string( "Part" ) )
                {
                    //this is a Part
                    ves::open::xml::cad::CADPartPtr newPart( new ves::open::xml::cad::CADPart() );
                    //VE_XML::VE_CAD::CADPart newPart;// = new VE_XML::VE_CAD::CADPart();
                    newPart->SetObjectFromXMLData( cadNode );
                    newPart->SetParent( mUuid );
                    mChildren.push_back( newPart );
                }
                else
                {
                    std::cout << "ERROR!" << std::endl;
                    std::cout << "Unknown node type:"
                    << tmpNodeType << std::endl;
                }
            }
        }
    }
}
///////////////////////////////////////////////////////////
CADAssembly::CADAssembly( CADAssembly& rhs, bool clone )
        : ves::open::xml::cad::CADNode( rhs, clone )
{

    mAssociatedDataset = rhs.mAssociatedDataset;
    for( size_t i = 0; i < rhs.mChildren.size(); i++ )
    {
        mChildren.push_back( boost::dynamic_pointer_cast<CADPart>(
            XMLObjectFactory::Instance()->CreateXMLObjectCopy(
               rhs.mChildren.at( i ) ) ) );
    }
}
///////////////////////////////////////////////////////////
CADAssembly& CADAssembly::operator=( const CADAssembly& rhs )
{
    if( this != &rhs )
    {
        ves::open::xml::cad::CADNode::operator =( rhs );

        mChildren.clear();

        for( size_t i = 0; i < rhs.mChildren.size(); i++ )
        {
            mChildren.push_back( rhs.mChildren.at( i ) );
        }
        mAssociatedDataset = rhs.mAssociatedDataset;
    }
    return *this;
}

