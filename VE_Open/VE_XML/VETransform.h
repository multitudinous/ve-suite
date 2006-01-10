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
 * File:          $RCSfile: VETransform.h,v $
 * Date modified: $Date: 2005-08-24 19:07:18 -0500 (Wed, 24 Aug 2005) $
 * Version:       $Rev: 2970 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef _XML_VE_TRANSFORM_H_
#define _XML_VE_TRANSFORM_H_
#include <string>
#include "VE_Installer/include/VEConfig.h"
#include "VE_Open/VE_XML/VEXMLObject.h"

#include <xercesc/dom/DOM.hpp>

#include <iostream>
namespace VE_XML
{
   class VEFloatArray;
}

namespace VE_XML
{
class VE_XML_EXPORTS VETransform : public VEXMLObject
{
public:
   VETransform( DOMDocument* rootDoc );
   virtual ~VETransform();
   VETransform( const VETransform& );
   //equal operator
   VETransform& operator= ( const VETransform& );

   /*
   void SetTranslation( float* );
   void SetScale( float* );
   //degrees, H-P-R (Z-X-Y)
   void SetRotation( float* );
   */
   
   //create the transform from xml input
   virtual void SetObjectFromXMLData( DOMNode* );
   
   
   /*
   float* GetTranslation( void );
   float* GetScale( void );
   float* GetRotation( void );
   */

   // Get float for translations
   VEFloatArray* GetTranslationArray( void );
   // Get float for scale
   VEFloatArray* GetScaleArray( void );
   // Get float for rotation
   VEFloatArray* GetRotationArray( void );
    
   // Set float for translations
   void SetTranslationArray( VEFloatArray* );
   // Set float for scale
   void SetScaleArray( VEFloatArray* );
   // Set float for rotation
   void SetRotationArray( VEFloatArray* );
    
protected:
   virtual void _updateVEElement( std::string );
   /*
   float* _translation;
   float* _scale;
   float* _rotation;
   */
   VEFloatArray* translationArray;
   VEFloatArray* scaleArray;
   VEFloatArray* rotationArray;
};
}
#endif// _XML_VE_TRANSFORM_H_
