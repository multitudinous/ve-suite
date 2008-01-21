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
#ifndef UNIFORM_H_
#define UNIFORM_H_
#include <ves/open/xml/XMLObject.h>
#include <ves/VEConfig.h>
#include <xercesc/dom/DOM.hpp>
#include <string>
#include <vector>

/*!\file Uniform.h
  Shader Uniform API
  */
/*!\class VE_Shader::Uniform
 * Class representing uniform variables for use in Shaders
 */

/*!\namespace VE_Shader
 * Contains classes that describe GLSL shaders and programs.
 */

namespace ves
{
namespace open
{
namespace xml
{
namespace shader
{
class VE_SHADER_EXPORTS Uniform : public ves::open::xml::XMLObject
{
public:
    ///Constructor
    Uniform();
    virtual ~Uniform();
    ///Copy constructor
    Uniform( const Uniform& rhs );

    ///Set the type of uniform
    ///Valid types:
    ///Float,Int,Bool
    ///\param type The type of data.
    void SetType( std::string type );

    ///Set the size of.
    ///Valid values range from 1-4
    ///\param uniformSize The size of the uniform (vector length)
    void SetSize( unsigned int uniformSize );

    ///Set the name.
    ///\param name The name of the uniform
    void SetName( std::string name );

    ///The texuture unit of the sampler if type is a sampler.
    ///\param tUnit The texture unit.
    void SetTextureUnit( unsigned int tUnit );

    ///Set the value(s) of the uniform.
    ///NOTE: All values should be passed in
    ///as floats and will be converted and interpretted appropriately.
    ///\param newValues The new values for the uniform.
    void SetValues( std::vector<float> newValues );

    ///Set the internal data values from a given XML element.
    ///\param inputXML The element to extract the data from.
    void SetObjectFromXMLData( XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* inputXML );

    ///Get the type.
    std::string GetType();

    ///Get the name.
    std::string GetName();

    ///Get the size of the uniform vector
    size_t GetSize();

    ///Get the texture unit of the sampler
    unsigned int GetTextureUnit();

    ///Return the current values.
    ///Values are returned as floats but should be converted as
    ///needed.
    std::vector<float> GetValues();

    ///Equal operator
    Uniform& operator=( const Uniform& rhs );
protected:
    ///Internally update the XML data for this element.
    ///\param input The XML element information
    virtual void _updateVEElement( std::string input );

    ///Internally update the data for the size of the uniform.
    void _updateSize();
    ///Internally update the data for the name of the uniform.
    void _updateUniformName();
    ///Internally update the data type of the uniform.
    void _updateUniformType();
    ///Internally update the values of the uniform.
    void _updateValues();
    ///Internally update the data for the texture unit.
    void _updateTextureUnit();

    std::string _type;///<The dataType.
    size_t _variableSize;///<The size.
    std::string _name;///<The name.
    std::vector<float> _values;///<The value of the uniform
    unsigned int _textureUnit;///<Optional texture unit if the data is a sampler.
};
}
template<>
inline XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* XMLObject::SetSubElement( const std::string subElementTagName, ves::open::xml::shader::Uniform* val )
{
    val->SetOwnerDocument( _rootDocument );
    XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* childElement = val->GetXMLData( subElementTagName );
    _veElement->appendChild( childElement );
    return childElement;
}
}
}
}
#endif //UNIFORM_H_
