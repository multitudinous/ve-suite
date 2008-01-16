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
#ifndef CAD_NODE_ANIMATION_H
#define CAD_NODE_ANIMATION_H
#include <ves/open/xml/cad/CADNodeAnimationPtr.h>
#include <ves/open/xml/XMLObject.h>
#include <xercesc/dom/DOM.hpp>
#include <string>

/*!\file CADNodeAnimation.h
  CADNodeAnimation API
  */
/*!\class VE_XML::VE_CAD::CADNodeAnimation
 * This class holds data for describing animations for a CADNode.
 */

namespace ves
{
namespace open
{
namespace xml
{
namespace cad
{
class VE_CAD_EXPORTS CADNodeAnimation: public ves::open::xml::XMLObject
{
public:
    ///Constructor
    CADNodeAnimation();
    virtual ~CADNodeAnimation();

    ///Set the source of animation file .
    ///\param attributeType The type of attribute.
    void SetAnimationFileName( std::string fileName );

    ///Set the source of animation file based on file extension.
    ///Currently valid sources are "osg" "txt".
    ///\param fileSourceType The type of attribute.
    void SetFileType( std::string fileSourceType );

    ///Set the name of the animation
    ///\param name The name of the animation
    void SetAnimationName( std::string name );

    ///Set the play mode.
    ///Valid types are "Loop" or "Once".
    ///\param playMode The play mode for the animation.
    void SetPlayMode( std::string playMode );

    ///Set the object from XML data
    ///\param xmlNode Node to set this object from
    virtual void SetObjectFromXMLData( XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlNode );

    ///Specify if we have a header
    ///\param hasHeader The header flag
    void SetHasHeader( bool hasHeader );

    ///Set the number of lines in the header, if it exists
    ///\param nHeaderLines Number of lines the header contains
    void SetNumberOfHeaderLines( unsigned int nHeaderLines );

    ///Get the file type.
    std::string GetFileType();

    ///Get the name of this animation.
    std::string GetAnimationName();

    ///The play mode of the animation.
    std::string GetPlayMode();

    ///Get the name of animation file.
    std::string GetAnimationFileName();

    ///Check for a header
    bool HasHeader();

    ///Get the number of lines in the header
    unsigned int GetNumberOfHeaderLines();

    ///Copy constructor
    CADNodeAnimation( const CADNodeAnimation& rhs );

    ///Equal operator
    CADNodeAnimation& operator=( const CADNodeAnimation& rhs );

protected:

    ///Internally update the XML data for this element.
    ///\param input The XML element information
    virtual void _updateVEElement( std::string input );

    std::string _fileSourceType;///<The type of attribute
    std::string _animationFileName;///<The actual name of the file.
    std::string _playMode;///<The play mode of the animation.
    std::string _name;///<The name of the animation.

    bool _hasHeader;///<Flag specifying header lines in file
    unsigned int _numberOfHeaderLines;///<The number of line in the header
};
}
template<>
inline XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* XMLObject::SetSubElement( const std::string subElementTagName, ves::open::xml::cad::CADNodeAnimation* val )
{
    val->SetOwnerDocument( _rootDocument );
    XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* childElement = val->GetXMLData( subElementTagName );
    _veElement->appendChild( childElement );
    return childElement;
}
}
}
}
#endif// CAD_NODE_ANIMATION_H
