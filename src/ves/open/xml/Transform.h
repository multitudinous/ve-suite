/*************** <auto-copyright.pl BEGIN do not edit this line> **************
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
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef _XML_VE_TRANSFORM_H_
#define _XML_VE_TRANSFORM_H_
/*!\file Transform.h
  Transform data
  */
/*!\class VE_XML::Transform
 * This class manages transform information. It contains 3
 * information in 3 FloatArray s which represent
 * Rotation,Scale and Translation.
 */
#include <string>

#include <ves/open/xml/XMLObject.h>

#include <xercesc/dom/DOM.hpp>

#include <iostream>
namespace ves
{
namespace open
{
namespace xml
{
class FloatArray;
}
}
}

namespace ves
{
namespace open
{
namespace xml
{
class VE_XML_EXPORTS Transform : public XMLObject
{
public:
    ///Constructor.
    Transform( );
    ///Destructor
    virtual ~Transform();
    ///Copy Constructor
    Transform( const Transform& );
    ///equal operator
    Transform& operator= ( const Transform& );


    ///Set the translation array for this transform
    ///\param translation The translation
    void SetTranslation( float* translation );

    ///Set the scale array for this transform
    ///\param scale The scale
    void SetScale( float* scale );

    ///Set the rotation array for this transform
    ///\param rotation The rotation angles in degrees\n
    ///rotation is specified in H-P-R (Z-X-Y)
    void SetRotation( float* rotation );


    ///Create the transform from xml input
    ///\param xmlInput The input XML data.
    virtual void SetObjectFromXMLData( XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlInput );


    /*
    float* GetTranslation( void );
    float* GetScale( void );
    float* GetRotation( void );
    */

    /// Get float for translations
    FloatArray* GetTranslationArray( void );
    /// Get float for scale
    FloatArray* GetScaleArray( void );
    /// Get float for rotation
    FloatArray* GetRotationArray( void );

    /// Set float for translations
    ///\param translation The FloatArray holding translation.
    void SetTranslationArray( FloatArray* translation );

    // Set float for scale
    ///\param scale The FloatArray holding scale.
    void SetScaleArray( FloatArray* scale );

    // Set float for rotation
    ///\param rotation The FloatArray holding rotation.
    void SetRotationArray( FloatArray* rotation );

protected:
    ///Internally update the XML data.
    ///\param tagName The tag name for this element.
    virtual void _updateVEElement( std::string tagName );
    /*
    float* _translation;
    float* _scale;
    float* _rotation;
    */
    FloatArray* translationArray;///<The FloatArray holding translation information.
    FloatArray* scaleArray;///<The FloatArray holding scale information.
    FloatArray* rotationArray;///<The FloatArray holding rotation information.
};
template<>
inline XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* XMLObject::SetSubElement( const std::string subElementTagName, Transform* val )
{
    val->SetOwnerDocument( _rootDocument );
    XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* childElement = val->GetXMLData( subElementTagName );
    _veElement->appendChild( childElement );
    return childElement;
}
}
}
}
#endif// _XML_VE_TRANSFORM_H_
