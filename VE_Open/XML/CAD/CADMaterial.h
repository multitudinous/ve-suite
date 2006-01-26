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
 * File:          $RCSfile: CADMaterial.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CAD_MATERIAL_H
#define CAD_MATERIAL_H
#include "VE_Open/XML/XMLObject.h"
#include <xercesc/dom/DOM.hpp>
#include <string>
#include <vector>

/*!\file CADMaterial.h
 * CADMaterial API
 */

/*! \class VE_CAD::CADMaterial
 * Class to represent a basic material.
 */
XERCES_CPP_NAMESPACE_USE
namespace VE_XML
{
   class FloatArray;
}

namespace VE_CAD
{
class VE_CAD_EXPORTS CADMaterial: public VE_XML::XMLObject
{
public:
   ///Constructor
   ///\param name The name of this material.
   CADMaterial(std::string name=std::string("Material"));
   ///Destructor
   virtual ~CADMaterial();

   ///Set the diffuse component
   ///\param diffuse RGBA diffuse property
   void SetDiffuseComponent(VE_XML::FloatArray* diffuse);

   ///Set the emissive component
   ///\param emissive RGBA emissive property
   void SetEmissiveComponent(VE_XML::FloatArray* emissive);

   ///Set the ambient component
   ///\param ambient RGBA ambient property
   void SetAmbientComponent(VE_XML::FloatArray* ambient);

   ///Set the specular reflection component
   ///\param specular RGBA specular property
   void SetSpecularComponent(VE_XML::FloatArray* specular);

   ///Set the "shininess" of this material
   ///\param shine value
   void SetShininess(float shine);

   ///Set the name of this material.
   ///\param name The name of the material.
   void SetMaterialName(std::string name);

   ///Set the object from XML data
   ///\param xmlNode Node to set this object from
   virtual void SetObjectFromXMLData( DOMNode* xmlNode);

   ///Set the face that this material applies to.
   ///\param faceToApplyTo The face that this material applies to.
   void SetFace(std::string faceToApplyTo);

   ///Set the color mode.
   ///Valid options are:
   ///Ambient
   ///Diffuse
   ///Specular
   ///Emission
   ///Ambient_and_Diffuse
   ///Off            
   ///\param colorMode The color mode of this material.
   void SetColorMode(std::string colorMode);
   
   
   ///Get the diffuse property
   VE_XML::FloatArray* GetDiffuse();

   ///Get the emissive property
   VE_XML::FloatArray* GetEmissive();

   ///Get the ambient property
   VE_XML::FloatArray* GetAmbient();

   ///Get the specular property
   VE_XML::FloatArray* GetSpecular();
  
   ///Get the shininess property
   double GetShininess();

   ///Get the material name.
   std::string GetMaterialName();

   ///Get the face that this material applies to.
   std::string GetFace();

   ///Get the color mode.
   std::string GetColorMode();
   
   ///Copy constructor
   CADMaterial(const CADMaterial& rhs);

   ///Equal operator
   CADMaterial& operator=(const CADMaterial& rhs);
protected:	
   

   ///Internally update the XML data for the material.
   ///\param input The new XML data for the material.
   virtual void _updateVEElement(std::string input);

   ///Internally update the XML data for the material shininess.
   void _updateShininess();

   ///Internally update the XML data for the material color properties.
   void _updateColorProperties();

   ///Internally update the XML data for the material name.
   void _updateMaterialName();

   ///Internally update the XML data for the material face.
   void _updateMaterialFace();

   ///Internally update the XML data for the material color mode.
   void _updateColorMode();

   VE_XML::FloatArray* _kDiffuse;///< Diffuse component.
   VE_XML::FloatArray* _kEmissive;///< Emmisive component.
   VE_XML::FloatArray* _ambient;///< Ambient component.
   VE_XML::FloatArray* _specular;///< Specular component.
   std::string _materialName;///< Name of this Material node.
   double _shininess;///< Shininess of the material
   std::string _colorMode;///< Color mode of this material
   std::string _face;///< Face that this material is applied to.
};
}
#endif //CAD_MATERIAL_H
