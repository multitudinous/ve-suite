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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/SceneGraph/Utilities/Attribute.h"
using namespace VE_SceneGraph::Utilities;
using namespace VE_XML::VE_CAD;
using namespace VE_XML::VE_Shader;

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
#include "VE_Xplorer/SceneGraph/Utilities/MaterialHelper.h"
#include "VE_Xplorer/SceneGraph/Utilities/ShaderHelper.h"

#ifdef _OSG
//why is this needed
#include <osg/Material>
#include <osg/BlendFunc>

//#include <osg/Texture2D>
#include <osg/TextureCubeMap>
#include <osg/TextureRectangle>

#include <osgDB/Registry>
#include <osgDB/Input>
#include <osgDB/Output>

#include <set>

using namespace osg;
using namespace osgDB;
using namespace std;
bool Attribute_readLocalData(Object& obj, Input& fr);
bool Attribute_writeLocalData(const Object& obj, Output& fw);

bool Attribute_matchModeStr(const char* str,StateAttribute::GLModeValue& mode);
const char* Attribute_getModeStr(StateAttribute::GLModeValue mode);

bool Attribute_matchRenderBinModeStr(const char* str,StateSet::RenderBinMode& mode);
const char* Attribute_getRenderBinModeStr(StateSet::RenderBinMode mode);

// register the read and write functions with the osgDB::Registry.
RegisterDotOsgWrapperProxy g_AttributeFuncProxy
(
    new VE_SceneGraph::Utilities::Attribute(),
    "Attribute",
    "Object Attribute",
    &Attribute_readLocalData,
    &Attribute_writeLocalData,
    DotOsgWrapper::READ_AND_WRITE
);
//
// Set up the maps from name to GLMode and visa versa.
//
typedef std::map<std::string,StateAttribute::GLMode>    GLNameToGLModeMap;
typedef std::map<StateAttribute::GLMode,std::string>    GLModeToGLNameMap;
typedef std::set<StateAttribute::GLMode>                TextureGLModeSet;

GLNameToGLModeMap s_GLNameToGLModeMap;
GLModeToGLNameMap s_GLModeToGLNameMap;
TextureGLModeSet s_TextureGLModeSet;

#define ADD_NAME(name,mode) s_GLNameToGLModeMap[name]=mode; s_GLModeToGLNameMap[mode]=name;

void initGLNames()
{
    static bool first_time = true;
    if (!first_time) return;
    
    ADD_NAME("GL_ALPHA_TEST",GL_ALPHA_TEST)
    ADD_NAME("GL_BLEND",GL_BLEND)
    ADD_NAME("GL_COLOR_MATERIAL",GL_COLOR_MATERIAL)
    ADD_NAME("GL_CULL_FACE",GL_CULL_FACE)
    ADD_NAME("GL_DEPTH_TEST",GL_DEPTH_TEST)
    ADD_NAME("GL_FOG",GL_FOG)
    ADD_NAME("GL_LIGHTING",GL_LIGHTING)
    ADD_NAME("GL_POINT_SMOOTH",GL_POINT_SMOOTH)
    ADD_NAME("GL_LINE_STIPPLE",GL_LINE_STIPPLE)
    ADD_NAME("GL_POLYGON_OFFSET_FILL",GL_POLYGON_OFFSET_FILL)
    ADD_NAME("GL_POLYGON_OFFSET_LINE",GL_POLYGON_OFFSET_LINE)
    ADD_NAME("GL_POLYGON_OFFSET_POINT",GL_POLYGON_OFFSET_POINT)
    ADD_NAME("GL_COLOR_SUM",GL_COLOR_SUM);
    
    ADD_NAME("GL_TEXTURE_1D",GL_TEXTURE_1D)
    ADD_NAME("GL_TEXTURE_2D",GL_TEXTURE_2D)
    ADD_NAME("GL_TEXTURE_3D",GL_TEXTURE_3D)
    
    ADD_NAME("GL_TEXTURE_CUBE_MAP",GL_TEXTURE_CUBE_MAP);
    ADD_NAME("GL_TEXTURE_RECTANGLE",GL_TEXTURE_RECTANGLE);
    
    ADD_NAME("GL_TEXTURE_GEN_Q",GL_TEXTURE_GEN_Q)
    ADD_NAME("GL_TEXTURE_GEN_R",GL_TEXTURE_GEN_R)
    ADD_NAME("GL_TEXTURE_GEN_S",GL_TEXTURE_GEN_S)
    ADD_NAME("GL_TEXTURE_GEN_T",GL_TEXTURE_GEN_T)
    
    ADD_NAME("GL_STENCIL_TEST",GL_STENCIL_TEST)
    
    ADD_NAME("GL_CLIP_PLANE0",GL_CLIP_PLANE0);
    ADD_NAME("GL_CLIP_PLANE1",GL_CLIP_PLANE1);
    ADD_NAME("GL_CLIP_PLANE2",GL_CLIP_PLANE2);
    ADD_NAME("GL_CLIP_PLANE3",GL_CLIP_PLANE3);
    ADD_NAME("GL_CLIP_PLANE4",GL_CLIP_PLANE4);
    ADD_NAME("GL_CLIP_PLANE5",GL_CLIP_PLANE5);

    ADD_NAME("GL_LIGHT0",GL_LIGHT0);
    ADD_NAME("GL_LIGHT1",GL_LIGHT1);
    ADD_NAME("GL_LIGHT2",GL_LIGHT2);
    ADD_NAME("GL_LIGHT3",GL_LIGHT3);
    ADD_NAME("GL_LIGHT4",GL_LIGHT4);
    ADD_NAME("GL_LIGHT5",GL_LIGHT5);
    ADD_NAME("GL_LIGHT6",GL_LIGHT6);
    ADD_NAME("GL_LIGHT7",GL_LIGHT7);
    
    s_TextureGLModeSet.insert(GL_TEXTURE_1D);
    s_TextureGLModeSet.insert(GL_TEXTURE_2D);
    s_TextureGLModeSet.insert(GL_TEXTURE_3D);
    
    s_TextureGLModeSet.insert(GL_TEXTURE_CUBE_MAP);
    s_TextureGLModeSet.insert(GL_TEXTURE_RECTANGLE);
    
    s_TextureGLModeSet.insert(GL_TEXTURE_GEN_Q);
    s_TextureGLModeSet.insert(GL_TEXTURE_GEN_R);
    s_TextureGLModeSet.insert(GL_TEXTURE_GEN_S);
    s_TextureGLModeSet.insert(GL_TEXTURE_GEN_T);
  
    first_time = false;
}

bool Attribute_readLocalData(Object& obj, Input& fr)
{
    bool iteratorAdvanced = false;

    // note, StateSet replaced GeoState April 2001.
    Attribute& stateset = static_cast<Attribute&>(obj);

    initGLNames();
    
    // read the rendering hint value.    
    if (fr[0].matchWord("rendering_hint"))
    {
        if (fr[1].matchWord("DEFAULT_BIN"))
        {
            stateset.setRenderingHint(StateSet::DEFAULT_BIN);
            fr+=2;
            iteratorAdvanced = true;
        }
        else if (fr[1].matchWord("OPAQUE_BIN"))
        {
            stateset.setRenderingHint(StateSet::OPAQUE_BIN);
            fr+=2;
            iteratorAdvanced = true;
        }
        else if (fr[1].matchWord("TRANSPARENT_BIN"))
        {
            stateset.setRenderingHint(StateSet::TRANSPARENT_BIN);
            fr+=2;
            iteratorAdvanced = true;
        }
        else if (fr[1].isInt())
        {
            int value;
            fr[1].getInt(value);
            stateset.setRenderingHint(value);
            fr+=2;
            iteratorAdvanced = true;
        }
    }

    bool setRenderBinDetails=false;
    StateSet::RenderBinMode rbmode = stateset.getRenderBinMode();
    if (fr[0].matchWord("renderBinMode") && Attribute_matchRenderBinModeStr(fr[1].getStr(),rbmode))
    {
        setRenderBinDetails=true;
        fr+=2;
        iteratorAdvanced = true;
    }

    int binNumber = stateset.getBinNumber();
    if (fr[0].matchWord("binNumber") && fr[1].getInt(binNumber))
    {
        setRenderBinDetails=true;
        fr+=2;
        iteratorAdvanced = true;
    }

    std::string binName = stateset.getBinName();
    if (fr[0].matchWord("binName"))
    {
        setRenderBinDetails=true;
        binName = fr[1].getStr();
        
        fr+=2;
        iteratorAdvanced = true;
    }

    if (setRenderBinDetails)
    {
        stateset.setRenderBinDetails(binNumber,binName,rbmode);
    }


    bool readingMode = true;
    StateAttribute::GLModeValue value;
    while (readingMode)
    {

        readingMode=false;
        if (fr[0].isInt())
        {
            if (Attribute_matchModeStr(fr[1].getStr(),value))
            {
                int mode;
                fr[0].getInt(mode);
                
                if (s_TextureGLModeSet.find(mode)!=s_TextureGLModeSet.end())
                {
                    // remap to a texture unit.
                    stateset.setTextureMode(0,(StateAttribute::GLMode)mode,value);
                }
                else
                {
                    stateset.setMode((StateAttribute::GLMode)mode,value);
                }
                fr+=2;
                iteratorAdvanced = true;
                readingMode=true;
            }
        }
        else
        if (fr[0].getStr())
        {
            if (Attribute_matchModeStr(fr[1].getStr(),value))
            {
                GLNameToGLModeMap::iterator nitr = s_GLNameToGLModeMap.find(fr[0].getStr());
                if (nitr!=s_GLNameToGLModeMap.end())
                {
                    StateAttribute::GLMode mode = nitr->second;
                    if (s_TextureGLModeSet.find(mode)!=s_TextureGLModeSet.end())
                    {
                        // remap to a texture unit.
                        stateset.setTextureMode(0,mode,value);
                    }
                    else
                    {
                        stateset.setMode(mode,value);
                    }
                    fr+=2;
                    iteratorAdvanced = true;
                    readingMode=true;
                }
            }
        } 
    }

    // new code using osg::Registry's list of prototypes to loaded attributes.
	osg::Uniform* uniform = NULL;
    while((uniform=fr.readUniform())!=NULL)
    {
        stateset.addUniform(uniform);
        iteratorAdvanced = true;
    }


    // new code using osg::Registry's list of prototypes to loaded attributes.
    StateAttribute* attribute = NULL;
    while((attribute=fr.readStateAttribute())!=NULL)
    {
        if (attribute->isTextureAttribute())
        {
            // remap to be a texture attribute
            stateset.setTextureAttribute(0,attribute);
        }
        else
        {
            stateset.setAttribute(attribute);
        }
        iteratorAdvanced = true;
    }
    
    while(fr.matchSequence("textureUnit %i {"))
    {
        int entry = fr[0].getNoNestedBrackets();

        unsigned int unit=0;
        fr[1].getUInt(unit);
        fr+=3;
        
        while (!fr.eof() && fr[0].getNoNestedBrackets()>entry)
        {
            bool localIteratorAdvanced = false;
            
            bool readingMode = true;
            StateAttribute::GLModeValue value;
            while (readingMode)
            {
                readingMode=false;
                if (fr[0].isInt())
                {
                    if (Attribute_matchModeStr(fr[1].getStr(),value))
                    {
                        int mode;
                        fr[0].getInt(mode);
                        stateset.setTextureMode(unit,(StateAttribute::GLMode)mode,value);
                        fr+=2;
                        localIteratorAdvanced = true;
                        readingMode=true;
                    }
                }
                else
                if (fr[0].getStr())
                {
                    if (Attribute_matchModeStr(fr[1].getStr(),value))
                    {
                        GLNameToGLModeMap::iterator nitr = s_GLNameToGLModeMap.find(fr[0].getStr());
                        if (nitr!=s_GLNameToGLModeMap.end())
                        {
                            StateAttribute::GLMode mode = nitr->second;
                            stateset.setTextureMode(unit,mode,value);
                            fr+=2;
                            localIteratorAdvanced = true;
                            readingMode=true;
                        }
                    }
                } 
            }
            
            StateAttribute* attribute = NULL;
            while((attribute=fr.readStateAttribute())!=NULL)
            {
                stateset.setTextureAttribute(unit,attribute);
                localIteratorAdvanced = true;
            }
            
            if (!localIteratorAdvanced)
                fr.advanceOverCurrentFieldOrBlock();
        }
        
        // skip over trailing '}'
        ++fr;
        
        iteratorAdvanced = true;

    }
    
    
    

    return iteratorAdvanced;
}

// visual studio 6.0 doesn't appear to define std::max?!? So do our own here.. 
template<class T>
T mymax(const T& a,const T& b)
{
    return (((a) > (b)) ? (a) : (b));
}

bool Attribute_writeLocalData(const Object& obj, Output& fw)
{

    const Attribute& stateset = static_cast<const Attribute&>(obj);

    initGLNames();

    // write the rendering hint value.    
    fw.indent()<<"rendering_hint ";
    switch(stateset.getRenderingHint())
    {
    case(StateSet::DEFAULT_BIN):
        fw<<"DEFAULT_BIN"<< std::endl;
        break;    
    case(StateSet::OPAQUE_BIN):
        fw<<"OPAQUE_BIN"<< std::endl;
        break;    
    case(StateSet::TRANSPARENT_BIN):
        fw<<"TRANSPARENT_BIN"<< std::endl;
        break;    
    default:
        fw<<stateset.getRenderingHint()<< std::endl;
        break;    
    }

    fw.indent()<<"renderBinMode "<<Attribute_getRenderBinModeStr(stateset.getRenderBinMode())<< std::endl;
    if (stateset.getRenderBinMode()!=StateSet::INHERIT_RENDERBIN_DETAILS)
    {
        fw.indent()<<"binNumber "<<stateset.getBinNumber()<< std::endl;
        fw.indent()<<"binName "<<stateset.getBinName()<< std::endl;
    }


  const StateSet::ModeList& ml = stateset.getModeList();
  for(StateSet::ModeList::const_iterator mitr=ml.begin();
        mitr!=ml.end();
        ++mitr)
    {
         GLModeToGLNameMap::iterator nitr = s_GLModeToGLNameMap.find(mitr->first);
         if (nitr!=s_GLModeToGLNameMap.end())
         {
             fw.indent() << nitr->second << " " << Attribute_getModeStr(mitr->second) << std::endl;
         }
         else
         {
            // no name defined for GLMode so just pass its value to fw.
             fw.indent() << "0x" << hex << (unsigned int)mitr->first << dec <<" " << Attribute_getModeStr(mitr->second) << std::endl;
         }
    }
    
    const StateSet::UniformList& ul = stateset.getUniformList();
    for(StateSet::UniformList::const_iterator uitr=ul.begin();
        uitr!=ul.end();
        ++uitr)
    {
        fw.writeObject(*(uitr->second.first));
    }

    const StateSet::AttributeList& sl = stateset.getAttributeList();
    for(StateSet::AttributeList::const_iterator sitr=sl.begin();
        sitr!=sl.end();
        ++sitr)
    {
        fw.writeObject(*(sitr->second.first));
    }

    
    const StateSet::TextureModeList& tml = stateset.getTextureModeList();    
    const StateSet::TextureAttributeList& tal = stateset.getTextureAttributeList();
    unsigned int maxUnit = mymax(tml.size(),tal.size());
    for(unsigned int unit=0;unit<maxUnit;++unit)
    {
        fw.indent()<<"textureUnit "<<unit<<" {"<< std::endl;
        fw.moveIn();
        
        if (unit<tml.size())
        {
            const StateSet::ModeList& ml = tml[unit];
            for(StateSet::ModeList::const_iterator mitr=ml.begin();
                mitr!=ml.end();
                ++mitr)
            {
                 GLModeToGLNameMap::iterator nitr = s_GLModeToGLNameMap.find(mitr->first);
                 if (nitr!=s_GLModeToGLNameMap.end())
                 {
                     fw.indent() << nitr->second << " " << Attribute_getModeStr(mitr->second) << std::endl;
                 }
                 else
                 {
                    // no name defined for GLMode so just pass its value to fw.
                    fw.indent() << "0x" << hex << (unsigned int)mitr->first << dec <<" " << Attribute_getModeStr(mitr->second) << std::endl;
                 }
            }
        }
        
        if (unit<tal.size())
        {
            const StateSet::AttributeList& sl = tal[unit];
            for(StateSet::AttributeList::const_iterator sitr=sl.begin();
                sitr!=sl.end();
                ++sitr)
            {
                fw.writeObject(*(sitr->second.first));
            }
        }
        
        fw.moveOut();
        fw.indent()<<"}"<< std::endl;
    }

    return true;
}


bool Attribute_matchModeStr(const char* str,StateAttribute::GLModeValue& mode)
{
    if (strcmp(str,"INHERIT")==0) mode = StateAttribute::INHERIT;
    else if (strcmp(str,"ON")==0) mode = StateAttribute::ON;
    else if (strcmp(str,"OFF")==0) mode = StateAttribute::OFF;
    else if (strcmp(str,"OVERRIDE_ON")==0) mode = StateAttribute::OVERRIDE|StateAttribute::ON;
    else if (strcmp(str,"OVERRIDE_OFF")==0) mode = StateAttribute::OVERRIDE|StateAttribute::OFF;
    else if (strcmp(str,"OVERRIDE|ON")==0) mode = StateAttribute::OVERRIDE|StateAttribute::ON;
    else if (strcmp(str,"OVERRIDE|OFF")==0) mode = StateAttribute::OVERRIDE|StateAttribute::OFF;
    else if (strcmp(str,"PROTECTED|ON")==0) mode = StateAttribute::PROTECTED|StateAttribute::ON;
    else if (strcmp(str,"PROTECTED|OFF")==0) mode = StateAttribute::PROTECTED|StateAttribute::OFF;
    else if (strcmp(str,"PROTECTED|OVERRIDE|ON")==0) mode = StateAttribute::PROTECTED|StateAttribute::OVERRIDE|StateAttribute::ON;
    else if (strcmp(str,"PROTECTED|OVERRIDE|OFF")==0) mode = StateAttribute::PROTECTED|StateAttribute::OVERRIDE|StateAttribute::OFF;
    else return false;
    return true;
}


const char* Attribute_getModeStr(StateAttribute::GLModeValue value)
{
    switch(value)
    {
        case(StateAttribute::INHERIT): return "INHERIT";
        case(StateAttribute::ON): return "ON";
        case(StateAttribute::OFF): return "OFF";
        case(StateAttribute::OVERRIDE|StateAttribute::ON): return "OVERRIDE|ON";
        case(StateAttribute::OVERRIDE|StateAttribute::OFF): return "OVERRIDE|OFF";
        case(StateAttribute::PROTECTED|StateAttribute::ON): return "PROTECTED|ON";
        case(StateAttribute::PROTECTED|StateAttribute::OFF): return "PROTECTED|OFF";
        case(StateAttribute::PROTECTED|StateAttribute::OVERRIDE|StateAttribute::ON): return "PROTECTED|OVERRIDE|ON";
        case(StateAttribute::PROTECTED|StateAttribute::OVERRIDE|StateAttribute::OFF): return "PROTECTED|OVERRIDE|OFF";
    }
    return "";
}

bool Attribute_matchRenderBinModeStr(const char* str,StateSet::RenderBinMode& mode)
{
    if (strcmp(str,"INHERIT")==0) mode = StateSet::INHERIT_RENDERBIN_DETAILS;
    else if (strcmp(str,"USE")==0) mode = StateSet::USE_RENDERBIN_DETAILS;
    else if (strcmp(str,"OVERRIDE")==0) mode = StateSet::OVERRIDE_RENDERBIN_DETAILS;
    else if (strcmp(str,"ENCLOSE")==0) mode = StateSet::USE_RENDERBIN_DETAILS;
    else return false;
    return true;
}

const char* Attribute_getRenderBinModeStr(StateSet::RenderBinMode mode)
{
    switch(mode)
    {
        case(StateSet::INHERIT_RENDERBIN_DETAILS):  return "INHERIT";
        case(StateSet::USE_RENDERBIN_DETAILS):      return "USE";
        case(StateSet::OVERRIDE_RENDERBIN_DETAILS): return "OVERRIDE";
    }
    return "";
}
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
void Attribute::UpdateShaderUniform(VE_XML::VE_Shader::Uniform* uniformToUpdate)
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
////////////////////////////////////////////
void Attribute::CreateTransparencyStateSet()
{
#ifdef _OSG  
   ShaderHelper shaderHelper;
   shaderHelper.SetStateSet(this);
   shaderHelper.LoadTransparencyProgram();

   osg::ref_ptr<osg::BlendFunc> bf = new osg::BlendFunc;
   bf->setFunction(osg::BlendFunc::SRC_ALPHA, osg::BlendFunc::ONE_MINUS_SRC_ALPHA);
   setRenderingHint(osg::StateSet::TRANSPARENT_BIN);
   setRenderBinDetails(99,std::string("DepthSortedBin"));
   setMode(GL_BLEND,osg::StateAttribute::ON);
   setAttributeAndModes(bf.get(),osg::StateAttribute::ON);

#elif _PERFORMER
#endif
}
////////////////////////////////////////////////////////////////////////////
void Attribute::CreateStateSetFromAttribute(VE_XML::VE_CAD::CADAttribute* attribute)
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
