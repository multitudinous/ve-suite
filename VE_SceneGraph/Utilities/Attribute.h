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
 * File:          $RCSfile: cfdNode.h,v $
 * Date modified: $Date: 2006-01-10 11:21:30 -0600 (Tue, 10 Jan 2006) $
 * Version:       $Rev: 3470 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef VE_SCENE_GRAPH_ATTRIBUTE_H
#define VE_SCENE_GRAPH_ATTRIBUTE_H

#include <string>


#ifdef _PERFORMER
#include <Performer/pr/pfMaterial.h>
#include <Performer/pf.h>
#include <Performer/pfdu.h>
#include <Performer/pfutil.h>
#include <Performer/pf/pfNode.h>
#elif _OSG
#include <osg/StateSet>
#include <osg/ref_ptr>
#elif _OPENSG
#endif
#include "VE_Installer/include/VEConfig.h"
namespace VE_CAD
{
   class CADAttribute;
}
namespace VE_Shader
{
   class Uniform;
}
namespace VE_SceneGraph
{
namespace Utilities
{
#ifdef _OSG
class VE_SCENEGRAPH_UTILS_EXPORTS Attribute : public osg::StateSet{
#elif _PERFORMER
class VE_SCENEGRAPH_UTILS_EXPORTS Attribute: public pfGeoState{
#endif
public:   
   ///Constructor
   Attribute();
#ifdef _OSG
   ///Copy Constructor for OpenSceneGraph object
   Attribute(const Attribute& pbQuad,
             const osg::CopyOp& copyop=osg::CopyOp::SHALLOW_COPY);

   ///OSG defines this macro
   META_Object(VE_SceneGraph::Utilities,Attribute);
   Attribute& operator=(const osg::StateSet& rhs);
#elif _PERFORMER
    Attribute(const Attribute& cfdSeq);
    //to make this a performer class
    static void init();

    static pfType* getClassType( void ){ return _classType; }
    Attribute& operator=(const Attribute& rhs);
    Attribute& operator=(const pfGeoState& rhs);

#endif 
   
   ///Destructor
   virtual ~Attribute();

   ///Create a StateSet from a CADAttribute.
   ///\param attribute The CADAttribute.
   void CreateStateSetFromAttribute(VE_CAD::CADAttribute* attribute);

   ///Update a Uniform value.
   ///\param uniformToUpdate The new uniform information.
   void UpdateShaderUniform(VE_Shader::Uniform* uniformToUpdate);

   ///Update a the components of a CADMaterial
   ///\param componentName The name of the component to update
   ///\param values The new values 
   ///\param face The material face 
   void UpdateMaterial(std::string componentName,std::string face,std::vector<double> values);

   ///Update a the modes of a CADMaterial
   ///\param type The mode type\n
   ///Color and Face are valid values. 
   ///\param mode The mode to set 
   void UpdateMaterialMode(std::string type,std::string mode);
protected:
#ifdef _PERFORMER
   static pfType* _classType;
#endif
};
}
}
#endif //VE_SCENE_GRAPH_ATTRIBUTE_H
