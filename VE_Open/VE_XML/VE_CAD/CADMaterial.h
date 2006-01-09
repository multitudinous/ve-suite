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
 * File:          $RCSfile: CADMaterial.h,v $
 * Date modified: $Date: 2005-07-11 13:47:16 -0500 (Mon, 11 Jul 2005) $
 * Version:       $Rev: 2653 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CAD_MATERIAL_H
#define CAD_MATERIAL_H

#include "VE_Installer/include/VEConfig.h"
#include "VE_Open/VE_XML/VEXMLObject.h"
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
namespace VE_XML{
   class VEFloatArray;
};
namespace VE_CAD{
   class VE_CAD_EXPORTS CADMaterial: public VE_XML::VEXMLObject{
public:
   ///Constructor
   ///\param rootDocument The root XML document of this material.
   ///\param name The name of this material.
   CADMaterial(DOMDocument* rootDocument,std::string name=std::string("Material"));
   ///Destructor
   virtual ~CADMaterial();

   ///Set the diffuse component
   ///\param diffuse RGBA diffuse property
   void SetDiffuseComponent(VE_XML::VEFloatArray* diffuse);

   ///Set the emissive component
   ///\param emissive RGBA emissive property
   void SetEmissiveComponent(VE_XML::VEFloatArray* emissive);

   ///Set the ambient component
   ///\param ambient RGBA ambient property
   void SetAmbientComponent(VE_XML::VEFloatArray* ambient);

   ///Set the specular reflection component
   ///\param specular RGBA specular property
   void SetSpecularComponent(VE_XML::VEFloatArray* specular);

   ///Set the "shininess" of this material
   ///\param shine value
   void SetShininess(float shine);

   ///Set the name of this material.
   ///\param name The name of the material.
   void SetMaterialName(std::string name);

   ///Set the object from XML data
   ///\param xmlNode Node to set this object from
   virtual void SetObjectFromXMLData( DOMNode* xmlNode);

   ///Get the diffuse property
   VE_XML::VEFloatArray* GetDiffuse();

   ///Get the emissive property
   VE_XML::VEFloatArray* GetEmissive();

   ///Get the ambient property
   VE_XML::VEFloatArray* GetAmbient();

   ///Get the specular property
   VE_XML::VEFloatArray* GetSpecular();
  
   ///Get the shininess property
   double GetShininess();

   ///Get the material name.
   std::string GetMaterialName();
   
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

   VE_XML::VEFloatArray* _kDiffuse;///< Diffuse component.
   VE_XML::VEFloatArray* _kEmissive;///< Emmisive component.
   VE_XML::VEFloatArray* _ambient;///< Ambient component.
   VE_XML::VEFloatArray* _specular;///< Specular component.
   std::string _materialName;///< Name of this Material node.
   double _shininess;///< Shininess of the material
};
}
#endif CAD_MATERIAL_H
