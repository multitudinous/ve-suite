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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifdef _OSG
#include <osg/StateSet>
#include <osg/Material>
#include <osg/BlendFunc>
#include <osg/ShadeModel>
#elif _PERFORMER
#endif

#include <ves/scenegraph/Utilities/MaterialHelper.h>

#include <ves/open/xml/cad/CADMaterial.h>
#include <ves/open/xmlFloatArray.h>
#include <string>
using namespace VE_XML::VE_CAD;
using namespace VE_SceneGraph::Utilities;
////////////////////////////////
//Construtor                  //
////////////////////////////////
MaterialHelper::MaterialHelper()
{
}
/////////////////////////////////
//Destructor                   //
/////////////////////////////////
MaterialHelper::~MaterialHelper()
{
}
////////////////////////////////////////////////////////////////
void MaterialHelper::LoadMaterial(VE_XML::VE_CAD::CADMaterial* material)
{
#ifdef _OSG
   if(!_material.valid())
   {
      _material = new osg::Material();
   }
   if(!_ss.valid())
   {
      _ss = new osg::StateSet();
   }
   else
   {
      _ss->clear();
   }

   std::string materialName = material->GetMaterialName();
   //std::cout<<"Loading material: "<<materialName<<std::endl;

   std::vector<double> diffuse = material->GetDiffuse()->GetArray();
   std::vector<double> ambient = material->GetAmbient()->GetArray();
   std::vector<double> specular = material->GetSpecular()->GetArray();
   std::vector<double> emmissive = material->GetEmissive()->GetArray();
   double opacity = material->GetOpacity();
   std::string face = material->GetFace();
   std::string colorMode = material->GetColorMode(); 
   float shininess = material->GetShininess();

   osg::Material::ColorMode cMode = 
      osg::Material::AMBIENT_AND_DIFFUSE;

   if(colorMode == std::string("Diffuse")){
      cMode = osg::Material::DIFFUSE;
   }else if(colorMode == std::string("Ambient")){
      osg::Material::AMBIENT;
   }else if(colorMode == std::string("Emissive")){
      osg::Material::EMISSION;
   }else if(colorMode == std::string("Specular")){
      osg::Material::SPECULAR;
   }else if(colorMode == std::string("Ambient_and_Diffuse")){
      osg::Material::AMBIENT_AND_DIFFUSE;
   }else if(colorMode == std::string("Off")){
      osg::Material::OFF;
   }else{
      std::cout<<"Unrecognized color mode: "<<colorMode<<std::endl;
      std::cout<<"SceneGraphBuilderSceneGraphCallback::Apply()"<<std::endl;
   }
   _material->setColorMode(cMode);

   osg::Material::Face faceToApply = osg::Material::FRONT;
   if(face == std::string("Front_and_Back")){
      faceToApply = osg::Material::FRONT_AND_BACK;
   }else if(face == std::string("Back")){
      faceToApply = osg::Material::BACK;
   }else if(face == std::string("Front")){
      faceToApply = osg::Material::FRONT;
   }else{
      std::cout<<"Unrecognized face: "<<face<<std::endl;
      std::cout<<"SceneGraphBuilderSceneGraphCallback::Apply()"<<std::endl;
   }
   _material->setDiffuse(faceToApply,
                            osg::Vec4(diffuse.at(0),diffuse.at(1),diffuse.at(2),opacity));
      
   _material->setAmbient(faceToApply,
                            osg::Vec4(ambient.at(0),ambient.at(1),ambient.at(2),opacity));

   _material->setEmission(faceToApply,
                            osg::Vec4(emmissive.at(0),emmissive.at(1),emmissive.at(2),opacity));
      
   _material->setSpecular(faceToApply,
                               osg::Vec4(specular.at(0),specular.at(1),specular.at(2),opacity));

   //_material->setName(materialName);
   _material->setShininess(faceToApply,shininess);
   _ss->setAttributeAndModes(_material.get(),osg::StateAttribute::ON);

   osg::ref_ptr<osg::BlendFunc> bf = new osg::BlendFunc;
      
   bf->setFunction(osg::BlendFunc::SRC_ALPHA, osg::BlendFunc::ONE_MINUS_SRC_ALPHA);
   
   if ( opacity == 1.0 ) 
   {
      _ss->setRenderingHint(osg::StateSet::OPAQUE_BIN);
      _ss->setMode(GL_BLEND,osg::StateAttribute::ON);
   }
   else
   {
     _ss->setRenderingHint(osg::StateSet::TRANSPARENT_BIN);
     _ss->setRenderBinDetails(99,std::string("DepthSortedBin"));
     _ss->setMode(GL_BLEND,osg::StateAttribute::ON);
   }
   _ss->setAttributeAndModes(bf.get(),osg::StateAttribute::ON|osg::StateAttribute::PROTECTED);
   _ss->setAttributeAndModes(_material.get(),osg::StateAttribute::ON|osg::StateAttribute::PROTECTED);
#elif _PERFORMER
   std::cout<<"Material Loader Not implemented for Performer version yet!!!!!!"<<std::endl;
#endif
}
#ifdef _OSG
/////////////////////////////////////////////////////////////////
osg::ref_ptr<osg::StateSet> MaterialHelper::GetMaterialStateSet()
{
   if(_ss.valid())
   {
      return _ss;
   }
   return 0;
}
/////////////////////////////////////////////////////////
void MaterialHelper::SetStateSet(osg::StateSet* material)
{
   _ss = material;
}
#elif _PERFORMER
#endif



