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
#include <ves/open/xml/cad/CADNodeAnimation.h>

#include <xercesc/dom/DOM.hpp>

using namespace ves::open::xml::cad;
using namespace ves::open::xml;

XERCES_CPP_NAMESPACE_USE
////////////////////////////////////
//Constructor                     //
////////////////////////////////////
CADNodeAnimation::CADNodeAnimation()
        : XMLObject()
{
    _fileSourceType = "OSG";
    _animationFileName = " ";
    _playMode = "Once";
    _name = "VE-Animation";
    _hasHeader = false;
    _numberOfHeaderLines = 0;
    SetObjectNamespace( "CAD" );
    SetObjectType( "CADNodeAnimation" );
}
/////////////////////////////////////
//Destructor                       //
/////////////////////////////////////
CADNodeAnimation::~CADNodeAnimation()
{}
/////////////////////////////////////////////////////////
void CADNodeAnimation::SetAnimationName( std::string name )
{
    _name = name;
}
/////////////////////////////////////////////////////////////////
void CADNodeAnimation::SetAnimationFileName( std::string fileName )
{
    _animationFileName = fileName;
    size_t period = fileName.rfind( "." );
    ///remove extension
    std::string outfileMinusExtension( fileName, period + 1, 3 );
}
//////////////////////////////////////////////////////////////
void CADNodeAnimation::SetFileType( std::string fileSourceType )
{
    _fileSourceType = fileSourceType;
}
////////////////////////////////////////////////////////
void CADNodeAnimation::SetPlayMode( std::string playMode )
{
    _playMode = playMode;
}
////////////////////////////////////////////////////////////////////////
void CADNodeAnimation::SetNumberOfHeaderLines( unsigned int nHeaderLines )
{
    _numberOfHeaderLines = nHeaderLines;
}
///////////////////////////////////////////////////
void CADNodeAnimation::SetHasHeader( bool hasHeader )
{
    _hasHeader = hasHeader;
}
///////////////////////////////////////////////////////////////////////////
void CADNodeAnimation::SetObjectFromXMLData( DOMNode* xmlNode )
{
    DOMElement* currentElement = 0;
    if( xmlNode->getNodeType() == DOMNode::ELEMENT_NODE )
    {
        currentElement = dynamic_cast<DOMElement*>( xmlNode );
    }

    if( currentElement )
    {
        //Get the attributes
        //GetAttribute(currentElement, "fileType",_fileSourceType);
        GetAttribute( currentElement, "playMode", _playMode );
        GetAttribute( currentElement, "hasHeader", _hasHeader );
        GetAttribute( currentElement, "numberHeaderLines", _numberOfHeaderLines );

        DOMElement* animationFile = GetSubElement( currentElement, "fileName", 0 );
        if( animationFile )
        {
            GetDataFromElement( animationFile, _animationFileName );
        }
        DOMElement* name = GetSubElement( currentElement, "name", 0 );
        if( name )
        {
            GetDataFromElement( name, _name );
        }
    }
}
//////////////////////////////////
bool CADNodeAnimation::HasHeader()
{
    return _hasHeader;
}
////////////////////////////////////////////////
std::string CADNodeAnimation::GetAnimationName()
{
    return _name;
}
///////////////////////////////////////////
std::string CADNodeAnimation::GetFileType()
{
    return _fileSourceType;
}
///////////////////////////////////////////
std::string CADNodeAnimation::GetPlayMode()
{
    return _playMode;
}
////////////////////////////////////////////////////
std::string CADNodeAnimation::GetAnimationFileName()
{
    return _animationFileName;
}
///////////////////////////////////////////////////////////////
CADNodeAnimation::CADNodeAnimation( const CADNodeAnimation& rhs )
        : XMLObject( rhs )
{
    _fileSourceType = rhs._fileSourceType;
    _animationFileName = rhs._animationFileName;
    _playMode = rhs._playMode;
    _name = rhs._name;
    _hasHeader = rhs._hasHeader;
    _numberOfHeaderLines = rhs._numberOfHeaderLines;
}
//////////////////////////////////////////////////////////////////////////
CADNodeAnimation& CADNodeAnimation::operator=( const CADNodeAnimation& rhs )
{
    if( this != &rhs )
    {
        XMLObject::operator =( rhs );
        _fileSourceType = rhs._fileSourceType;
        _animationFileName = rhs._animationFileName;
        _playMode = rhs._playMode;
        _name = rhs._name;
        _hasHeader = rhs._hasHeader;
        _numberOfHeaderLines = rhs._numberOfHeaderLines;
    }
    return *this;
}
//////////////////////////////////////////////////////////
void CADNodeAnimation::_updateVEElement( const std::string& input )
{
    SetSubElement( "fileName", _animationFileName );
    SetSubElement( "name", _name );
    SetAttribute( "fileType", _fileSourceType );
    SetAttribute( "playMode", _playMode );
    SetAttribute( "hasHeader", _hasHeader );
    SetAttribute( "numberOfHeaderLines", _numberOfHeaderLines );
}

