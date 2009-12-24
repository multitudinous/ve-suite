/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
#ifndef VE_SCENE_GRAPH_ATTRIBUTE_H
#define VE_SCENE_GRAPH_ATTRIBUTE_H

#include <string>
#include <vector>

#include <osg/StateSet>
#include <osg/ref_ptr>

#include <ves/open/xml/shader/UniformPtr.h>
#include <ves/open/xml/cad/CADAttributePtr.h>
#include <ves/VEConfig.h>


namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace util
{
class VE_SCENEGRAPH_UTILS_EXPORTS Attribute : public osg::StateSet
{
public:
    ///Constructor
    Attribute();
    ///Copy Constructor for OpenSceneGraph object
    Attribute( const Attribute& pbQuad,
               const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///OSG defines this macro
    META_Object( ves::xplorer::scenegraph::util, Attribute );
    Attribute& operator=( const osg::StateSet& rhs );

    ///Destructor
    virtual ~Attribute();

    ///Create a StateSet from a CADAttribute.
    ///\param attribute The CADAttribute.
    void CreateStateSetFromAttribute( ves::open::xml::cad::CADAttributePtr attribute );

    ///Sets this stateset to be the transparency shader.
    void CreateTransparencyStateSet();

    ///Update a Uniform value.
    ///\param uniformToUpdate The new uniform information.
    void UpdateShaderUniform( ves::open::xml::shader::UniformPtr uniformToUpdate );

    ///Update a the components of a CADMaterial
    ///\param componentName The name of the component to update
    ///\param values The new values
    ///\param face The material face
    void UpdateMaterial( std::string componentName, std::string face, std::vector<double> values );

    ///Update a the modes of a CADMaterial
    ///\param type The mode type\n
    ///Color and Face are valid values.
    ///\param mode The mode to set
    void UpdateMaterialMode( std::string type, std::string mode );
protected:

};
}
}
}
}

#endif //VE_SCENE_GRAPH_ATTRIBUTE_H
