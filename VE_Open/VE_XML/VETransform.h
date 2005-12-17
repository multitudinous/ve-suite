#ifndef _XML_VE_TRANSFORM_H_
#define _XML_VE_TRANSFORM_H_
#include <string>
#include "VE_Installer/include/VEConfig.h"
#include "VE_XML/VEXMLObject.h"

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
