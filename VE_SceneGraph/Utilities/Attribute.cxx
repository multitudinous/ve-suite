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
#include "VE_SceneGraph/Utilities/Attribute.h"

using namespace VE_SceneGraph::Utilities;
using namespace VE_CAD;
#ifdef _PERFORMER
#include <Performer/pf.h>
#include <Performer/pfdu.h>
#include <Performer/pfutil.h>
#include <Performer/pf/pfNode.h>
//Performer static member for performer compliance
//it allows performer to determine the class type

pfType* VE_SceneGraph::Utilities::Attribute::_classType = NULL;
//initialize our class w/ performer at run time
void VE_SceneGraph::Utilities::Attribute::init(void)
{
   if(_classType == 0)
   {
      //initialize the parent
      pfGeoState::init();
      //create the new class type
      _classType = new pfType(pfGeoState::getClassType(),"Attribute");
   }
}
#endif
#include "VE_Open/XML/CAD/CADAttribute.h"
#include "VE_SceneGraph/Utilities/MaterialHelper.h"
#include "VE_SceneGraph/Utilities/ShaderHelper.h"

#ifdef _OSG
//why is this needed
#include <osg/Material>
#include <osg/BlendFunc>
#elif _PERFORMER
#endif

//////////////////////
///Constructor      //
//////////////////////
Attribute::Attribute()
#ifdef _OSG
:osg::StateSet()
#elif _PERFORMER
:pfGeoState()
#endif
{
#ifdef _PERFORMER
   init();
   setType(_classType);
#endif

}
#ifdef _OSG
//////////////////////////////////////////////////////////
Attribute::Attribute(const Attribute& veAttribute,
                   const osg::CopyOp& copyop)
:osg::StateSet(veAttribute,copyop)
{
}
#endif
///////////////////////
Attribute::~Attribute()
{
}
////////////////////////////////////////////////////////////////////////////
void Attribute::CreateStateSetFromAttribute(VE_CAD::CADAttribute* attribute)
{
   std::string attributeType = attribute->GetAttributeType();
   bool blending = attribute->NeedsBlending();

#ifdef _OSG
   
#elif _PERFORMER
#endif
   if( attributeType == std::string("Material"))
   {
#ifdef _OSG
      MaterialHelper materialHelper;
      materialHelper.LoadMaterial(attribute->GetMaterial());
     
#elif _PERFORMER
#endif
   }
   else if( attributeType == std::string("Program"))
   {
#ifdef _OSG      
      osg::ref_ptr<osg::BlendFunc> bf = new osg::BlendFunc;
      bf->setFunction(osg::BlendFunc::SRC_ALPHA, osg::BlendFunc::ONE_MINUS_SRC_ALPHA);
   
      //probabaly need more params for the user to set but initially, we don't need that
      //so either enable "typical" blending (1-alpha) and bin appropriately.
      if (!blending) 
      {
         setRenderingHint(osg::StateSet::OPAQUE_BIN);
         setMode(GL_BLEND,osg::StateAttribute::ON);
      }
      else
      {
        setRenderingHint(osg::StateSet::TRANSPARENT_BIN);
        setRenderBinDetails(99,std::string("DepthSortedBin"));
        setMode(GL_BLEND,osg::StateAttribute::ON);
      }
      setAttributeAndModes(bf.get(),osg::StateAttribute::ON);
      
      ShaderHelper shaderHelper;
      shaderHelper.SetStateSet(this);
      shaderHelper.LoadGLSLProgram(attribute->GetGLSLProgram());

#elif _PERFORMER
#endif
   }
}
#ifdef _OSG
/////////////////////////////////////////////////////
Attribute& Attribute::operator=(const osg::StateSet& rhs)
{
   if(this != &rhs)
   {
      osg::StateSet::operator=(rhs);
   }
   return *this;
}
#endif
