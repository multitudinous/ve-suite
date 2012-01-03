/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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
#ifndef CAD_MATERIAL_H
#define CAD_MATERIAL_H
#include <ves/open/xml/cad/CADMaterialPtr.h>
#include <ves/open/xml/FloatArrayPtr.h>

#include <ves/open/xml/XMLObject.h>
#include <xercesc/dom/DOM.hpp>
#include <string>
#include <vector>

/*!\file CADMaterial.h
 * CADMaterial API
 */

/*! \class VE_XML::VE_CAD::CADMaterial
 * Class to represent a basic material.
 */
XERCES_CPP_NAMESPACE_USE

#include <vector>

namespace ves
{
namespace open
{
namespace xml
{
namespace cad
{
class VE_CAD_EXPORTS CADMaterial: public ves::open::xml::XMLObject
{
public:
    ///Constructor
    ///\param name The name of this material.
    CADMaterial( const std::string& name = std::string( "Material" ) );
    ///Destructor
    virtual ~CADMaterial();

    ///Set the diffuse component
    ///\param diffuse RGBA diffuse property
    void SetDiffuseComponent( ves::open::xml::FloatArrayPtr diffuse );

    ///Set the emissive component
    ///\param emissive RGBA emissive property
    void SetEmissiveComponent( ves::open::xml::FloatArrayPtr emissive );

    ///Set the ambient component
    ///\param ambient RGBA ambient property
    void SetAmbientComponent( ves::open::xml::FloatArrayPtr ambient );

    ///Set the specular reflection component
    ///\param specular RGBA specular property
    void SetSpecularComponent( ves::open::xml::FloatArrayPtr specular );

    ///Set the "shininess" of this material
    ///\param shine value
    void SetShininess( float shine );

    ///Set the name of this material.
    ///\param name The name of the material.
    void SetMaterialName( const std::string& name );

    ///Set the object from XML data
    ///\param xmlNode Node to set this object from
    virtual void SetObjectFromXMLData( DOMNode* xmlNode );

    ///Set the face that this material applies to.
    ///\param faceToApplyTo The face that this material applies to.
    void SetFace( const std::string& faceToApplyTo );

    ///Set a component of the material
    ///\param componentName The name of the component to set the values to.
    ///\param values The new values to use
    void SetComponent( const std::string& componentName, double* values );

    ///Set a component of the material
    ///\param componentName The name of the component to set the values to.
    ///\param values The new values to use
    void SetComponent( const std::string& componentName, std::vector<double> values );

    ///Set the color mode.
    ///Valid options are:
    ///Ambient
    ///Diffuse
    ///Specular
    ///Emission
    ///Ambient_and_Diffuse
    ///Off
    ///\param colorMode The color mode of this material.
    void SetColorMode( const std::string& colorMode );

    ///Set the overall opacity for this material
    ///\param  opacity The opacity value;
    void SetOpacity( double opacity );

    ///Get the opacity value
    double GetOpacity();

    ///Get the diffuse property
    ves::open::xml::FloatArrayPtr GetDiffuse();

    ///Get the emissive property
    ves::open::xml::FloatArrayPtr GetEmissive();

    ///Get the ambient property
    ves::open::xml::FloatArrayPtr GetAmbient();

    ///Get the specular property
    ves::open::xml::FloatArrayPtr GetSpecular();

    ///Get the shininess property
    double GetShininess();

    ///Get the material name.
    std::string GetMaterialName();

    ///Get the face that this material applies to.
    std::string GetFace();

    ///Get the color mode.
    std::string GetColorMode();

    ///Copy constructor
    CADMaterial( const CADMaterial& rhs );

    ///Equal operator
    CADMaterial& operator=( const CADMaterial& rhs );
protected:


    ///Internally update the XML data for the material.
    ///\param input The new XML data for the material.
    virtual void _updateVEElement( const std::string& input );

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

    ves::open::xml::FloatArrayPtr _kDiffuse;///< Diffuse component.
    ves::open::xml::FloatArrayPtr _kEmissive;///< Emmisive component.
    ves::open::xml::FloatArrayPtr _ambient;///< Ambient component.
    ves::open::xml::FloatArrayPtr _specular;///< Specular component.
    std::string _materialName;///< Name of this Material node.
    double _shininess;///< Shininess of the material
    std::string _colorMode;///< Color mode of this material
    std::string _face;///< Face that this material is applied to.
    double _opacity;///<Opacity value
};

}
}
}
}
#endif //CAD_MATERIAL_H
