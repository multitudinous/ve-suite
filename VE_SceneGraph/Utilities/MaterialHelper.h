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
 * File:          $RCSfile: MaterialHelper.h,v $
 * Date modified: $Date: 2005-11-03 17:22:13 -0600 (Thu, 03 Nov 2005) $
 * Version:       $Rev: 3259 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef MATERIAL_HELPER_H
#define MATERIAL_HELPER_H
/*!\file MaterialHelper.h
  MaterialHelper API
  */
/*!\class VE_SceneGraph::Utilities::MaterialHelper
 * Class that creates an OSG StateSet representing
 * a glsl program.
 */

///\todo Still need to implement the performer side
#ifdef _OSG
#include <osg/StateSet>
namespace osg
{
   class StateSet;
   class Material;
}
#elif _PERFORMER
#endif
namespace VE_CAD
{
   class CADMaterial;
}
#include <string>
///////////////////////////////////////////////////////////////////////
//this class is used to create a stateset representing a gl Material //
///////////////////////////////////////////////////////////////////////
namespace VE_SceneGraph
{
namespace Utilities
{
class MaterialHelper{
public:
   ///Constructor
   MaterialHelper();
   ///Destructor
   virtual ~MaterialHelper();

   ///Load and create the stateset from the input XML data
   void LoadMaterial(VE_CAD::CADMaterial* material);
#ifdef _OSG
   ///Get the created state set representing the shader
   osg::ref_ptr<osg::StateSet> GetMaterialStateSet();
#elif _PERFORMER
#endif
protected:
#ifdef _OSG
   osg::ref_ptr<osg::Material> _material;///<The GLSL program.
   osg::ref_ptr<osg::StateSet> _ss;///<The stateset representing the GLSL program.
#elif _PERFORMER
#endif
};
}
}
#endif// MATERIAL_HELPER_H
