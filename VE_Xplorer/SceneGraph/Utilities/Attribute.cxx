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
using namespace VE_Shader;

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
#include "VE_Open/XML/Shader/Uniform.h"
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
//////////////////////////////////////////////////////////////////////
void Attribute::UpdateMaterialMode(std::string type, std::string mode)
{
#ifdef _OSG
   osg::ref_ptr<osg::Material> material = dynamic_cast<osg::Material*>(this->getAttribute(osg::StateAttribute::MATERIAL));   
   if(material.valid())
   {
      if(type == "Color")
      {
         osg::Material::ColorMode colorMode = osg::Material::OFF;
         if(mode == "Ambient")
         {
            colorMode = osg::Material::AMBIENT;
         }
         else if(mode == "Diffuse")
         {
            colorMode = osg::Material::DIFFUSE;
         }
         else if(mode == "Emissive")
         {
            colorMode = osg::Material::EMISSION;
         }
         else if(mode == "Specular")
         {
            colorMode = osg::Material::SPECULAR;
         }
         else if(mode == "Ambient_and_Diffuse")
         {
           colorMode = osg::Material::AMBIENT_AND_DIFFUSE; 
         }
         else if(mode == "Off")
         {
           colorMode = osg::Material::OFF;
         }
         material->setColorMode(colorMode);
      }
      else if(type == "Face")
      {
      }
   }
#endif
}
////////////////////////////////////////////////////////////////////////
void Attribute::UpdateShaderUniform(VE_Shader::Uniform* uniformToUpdate)
{
#ifdef _OSG
   ShaderHelper shaderHelper;
   shaderHelper.SetStateSet(this);
   shaderHelper.UpdateUniform(uniformToUpdate);
#endif
}
////////////////////////////////////////////////////////////////
void Attribute::UpdateMaterial(std::string componentName,std::string face,
                               std::vector<double> values)
{
#ifdef _OSG
   osg::Material::Face faceMode = osg::Material::FRONT_AND_BACK;
   if(face == "Back")
   {
      faceMode = osg::Material::BACK;
   }
   else if(face == "Front")
   {
      faceMode = osg::Material::FRONT;
   }
   osg::ref_ptr<osg::Material> material = dynamic_cast<osg::Material*>(this->getAttribute(osg::StateAttribute::MATERIAL));   
   if(material.valid())
   {
      /*std::cout<<"Updating: "<<componentName<<" :(";
      std::cout<<values[0]<<",";
      std::cout<<values[1]<<",";
      std::cout<<values[2]<<",";
      std::cout<<values[3]<<")"<<std::endl;
     */ 
      if(componentName == "Diffuse")
      {
         material->setDiffuse(faceMode,osg::Vec4(values[0],values[1],values[2],values[3]));
      }
      else if(componentName == "Ambient")
      {
         material->setAmbient(faceMode,osg::Vec4(values[0],values[1],values[2],values[3]));
      }
      else if(componentName == "Specular")
      {
         material->setSpecular(faceMode,osg::Vec4(values[0],values[1],values[2],values[3]));
      }
      else if(componentName == "Emissive")
      {
         material->setEmission(faceMode,osg::Vec4(values[0],values[1],values[2],values[3]));
      }
      else if(componentName == "Opacity")
      {
         material->setAlpha(faceMode,values[0]);
      }
   }
   else 
   {
      std::cout<<"Node doesn't contian a material!!"<<std::endl;
   }
#endif
}
////////////////////////////////////////////////////////////////////////////
void Attribute::CreateStateSetFromAttribute(VE_CAD::CADAttribute* attribute)
{
   std::string attributeType = attribute->GetAttributeType();
   bool blending = attribute->NeedsBlending();

   if( attributeType == std::string("Material"))
   {
#ifdef _OSG
      MaterialHelper materialHelper;
      materialHelper.SetStateSet(this);
      materialHelper.LoadMaterial(attribute->GetMaterial());
#elif _PERFORMER
#endif
   }
   else if( attributeType == std::string("Program"))
   {
#ifdef _OSG      
      
      ShaderHelper shaderHelper;
      shaderHelper.SetStateSet(this);
      shaderHelper.LoadGLSLProgram(attribute->GetGLSLProgram());
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
