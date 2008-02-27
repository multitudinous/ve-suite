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

#include <ves/open/xml/model/Tag.h>
#include <ves/open/xml/model/Point.h>
XERCES_CPP_NAMESPACE_USE
using namespace ves::open::xml;
using namespace ves::open::xml::model;
////////////////////////////////////////////
//Constructor                             //
////////////////////////////////////////////
Tag::Tag()
        : XMLObject()
{
    SetObjectType( "Tag" );
    SetObjectNamespace( "Model" );
}
////////////////////////////////////////////////////////////////////////////////
Tag::~Tag()
{
    mTagPoints.clear();
}
////////////////////////////////////////////////////////////////////////////////
Tag::Tag( const Tag& input )
        : XMLObject( input )
{
    mTagText = input.mTagText;

    for( size_t i = 0; i < input.mTagPoints.size(); ++i )
    {
        mTagPoints.push_back( PointPtr( new Point( *( input.mTagPoints.at( i ) ) ) ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
Tag& Tag::operator=( const Tag& input )
{
    if( this != &input )
    {
        //biv-- make sure to call the parent =
        XMLObject::operator =( input );
        mTagText = input.mTagText;

        mTagPoints.clear();

        for( size_t i = 0; i < input.mTagPoints.size(); ++i )
        {
            mTagPoints.push_back( PointPtr( new Point( *( input.mTagPoints.at( i ) ) ) ) );
        }
    }
    return *this;
}
////////////////////////////////////////////////////////////////////////////////
void Tag::SetText( const std::string& text )
{
    mTagText = text;
}
////////////////////////////////////////////////////////////////////////////////
void Tag::_updateVEElement( const std::string& input )
{
    // write all the elements according to verg_model.xsd
    SetAttribute( "id", mUuid );
    SetSubElement( "tagText", mTagText );
    SetSubElements( "linkPoints", mTagPoints );
}
////////////////////////////////////////////////////////////////////////////////
const std::string& Tag::GetText( void )
{
    return mTagText;
}
////////////////////////////////////////////////////////////////////////////////
PointPtr Tag::GetPoint( size_t i )
{
    try
    {
        return mTagPoints.at( i );
    }
    catch ( ... )
    {
        std::cerr << "The element request is out of sequence."
        << " Please ask for a lower number point." << std::endl;
        return PointPtr();
    }
}
////////////////////////////////////////////////////////////////////////////////
void Tag::AddPoint( PointPtr newPoint )
{
    mTagPoints.push_back( newPoint );
}
////////////////////////////////////////////////////////////////////////////////
void Tag::SetObjectFromXMLData( DOMNode* element )
{
    DOMElement* currentElement = 0;
    if( element->getNodeType() == DOMNode::ELEMENT_NODE )
    {
        currentElement = static_cast< DOMElement* >( element );
    }

    if( !currentElement )
    {
        return;
    }

    //get variables by tags
    DOMElement* dataValueStringName = 0;
    dataValueStringName = GetSubElement( currentElement, "tagText", 0 );
    GetDataFromElement( dataValueStringName, mTagText );
    // for Tag points
    unsigned int numberOfPoints =
        currentElement->getElementsByTagName(
            Convert( "linkPoints" ).toXMLString() )->getLength();

    for( unsigned int i = 0; i < numberOfPoints; ++i )
    {
        dataValueStringName = GetSubElement( currentElement, "linkPoints", i );
        mTagPoints.push_back( PointPtr( new Point() ) );
        mTagPoints.back()->SetObjectFromXMLData( dataValueStringName );
    }

    //Setup uuid for model element
    {
        GetAttribute( currentElement, "id", mUuid );
    }
}
////////////////////////////////////////////////////////////////////////////////
