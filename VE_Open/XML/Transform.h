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
 * File:          $RCSfile: Transform.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
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

#include "VE_Open/XML/XMLObject.h"

#include <xercesc/dom/DOM.hpp>

#include <iostream>
namespace VE_XML
{
   class FloatArray;
}

namespace VE_XML
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

   /*
   void SetTranslation( float* );
   void SetScale( float* );
   //degrees, H-P-R (Z-X-Y)
   void SetRotation( float* );
   */
   
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
   void SetTranslationArray( FloatArray* translation);

   // Set float for scale
   ///\param scale The FloatArray holding scale.
   void SetScaleArray( FloatArray* scale );
   
   // Set float for rotation
   ///\param rotation The FloatArray holding rotation.
   void SetRotationArray( FloatArray* rotation);
    
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
}
#endif// _XML_VE_TRANSFORM_H_
