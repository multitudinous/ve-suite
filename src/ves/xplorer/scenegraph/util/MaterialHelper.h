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
#ifndef MATERIAL_HELPER_H
#define MATERIAL_HELPER_H
/*!\file MaterialHelper.h
  MaterialHelper API
  */
/*!\class ves::xplorer::scenegraph::util::MaterialHelper
 * Class that creates an OSG StateSet representing
 * a glsl program.
 */
#include <ves/VEConfig.h>

///\todo Still need to implement the performer side
#include <osg/StateSet>
namespace osg
{
class StateSet;
class Material;
}
#include <ves/open/xml/cad/CADMaterial.h>
#include <string>
///////////////////////////////////////////////////////////////////////
//this class is used to create a stateset representing a gl Material //
///////////////////////////////////////////////////////////////////////
namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace util
{
class VE_SCENEGRAPH_UTILS_EXPORTS MaterialHelper
{
public:
    ///Constructor
    MaterialHelper();
    ///Destructor
    virtual ~MaterialHelper();

    ///Load and create the stateset from the input XML data
    void LoadMaterial( ves::open::xml::cad::CADMaterialPtr material );
    ///The state set that we want to load the material into
    ///\param materialThe state set representing the material.
    void SetStateSet( osg::StateSet* material );

    ///Get the created state set representing the shader
    osg::ref_ptr<osg::StateSet> GetMaterialStateSet();
protected:
    osg::ref_ptr<osg::Material> m_material;///<The GLSL program.
    osg::ref_ptr<osg::StateSet> m_ss;///<The stateset representing the GLSL program.
};
}
}
}
}
#endif// MATERIAL_HELPER_H
