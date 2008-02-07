// --- My Includes --- //
#include "VertFrag.h"
#include "Shaders.h"

// --- OSG Includes --- //
#include <osg/Node>
#include <osg/StateSet>
#include <osg/Texture2D>
#include <osg/TextureCubeMap>

#include <osgDB/ReadFile>

// --- C/C++ Libraries --- //
#include <string>

using namespace hyperlab;

////////////////////////////////////////////////////////////////////////////////
Shaders::Shaders()
{
   tcm = new osg::TextureCubeMap;

   ReadTextures();
}
////////////////////////////////////////////////////////////////////////////////
Shaders::~Shaders()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::ReadTextures()
{
   imageList.push_back(osgDB::readImageFile("./Textures/Decoration.tga"));
   imageList.push_back(osgDB::readImageFile("./Textures/WallMap.tga"));

   tcm->setWrap(osg::Texture::WRAP_S,osg::Texture::CLAMP_TO_EDGE);
   tcm->setWrap(osg::Texture::WRAP_T,osg::Texture::CLAMP_TO_EDGE);
   tcm->setWrap(osg::Texture::WRAP_R,osg::Texture::CLAMP_TO_EDGE);
   tcm->setFilter(osg::Texture::MIN_FILTER,osg::Texture::LINEAR);
   
   tcm->setImage(osg::TextureCubeMap::POSITIVE_X,osgDB::readImageFile("./Textures/CubeMap/Right.tga"));
   tcm->setImage(osg::TextureCubeMap::NEGATIVE_X,osgDB::readImageFile("./Textures/CubeMap/Left.tga"));
   tcm->setImage(osg::TextureCubeMap::POSITIVE_Y,osgDB::readImageFile("./Textures/CubeMap/Top.tga"));
   tcm->setImage(osg::TextureCubeMap::NEGATIVE_Y,osgDB::readImageFile("./Textures/CubeMap/Bottom.tga"));
   tcm->setImage(osg::TextureCubeMap::POSITIVE_Z,osgDB::readImageFile("./Textures/CubeMap/Back.tga"));
   tcm->setImage(osg::TextureCubeMap::NEGATIVE_Z,osgDB::readImageFile("./Textures/CubeMap/Front.tga"));
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::Base(osg::ref_ptr<osg::Node> node)
{
   osg::ref_ptr<osg::StateSet> stateset=node->getOrCreateStateSet();
   osg::ref_ptr<osg::Program> program=new osg::Program;

   osg::ref_ptr<osg::Shader> vertex_shader=new osg::Shader(osg::Shader::VERTEX,base_vertex);
   program->addShader(vertex_shader.get());

   osg::ref_ptr<osg::Shader> fragment_shader=new osg::Shader(osg::Shader::FRAGMENT,base_fragment);
   program->addShader(fragment_shader.get());

   stateset->setAttribute(program.get());
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::Phong(osg::ref_ptr<osg::Node> node)
{
   osg::ref_ptr<osg::StateSet> stateset=node->getOrCreateStateSet();
   osg::ref_ptr<osg::Program> program=new osg::Program;

   osg::ref_ptr<osg::Shader> vertex_shader=new osg::Shader(osg::Shader::VERTEX,phong_vertex);
   program->addShader(vertex_shader.get());

   osg::ref_ptr<osg::Shader> fragment_shader=new osg::Shader(osg::Shader::FRAGMENT,phong_fragment);
   program->addShader(fragment_shader.get());

   stateset->setAttribute(program.get());
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::Texture(int tex_num,osg::ref_ptr<osg::Node> node)
{
   osg::ref_ptr<osg::StateSet> stateset=node->getOrCreateStateSet();
   osg::ref_ptr<osg::Program> program=new osg::Program;

   osg::ref_ptr<osg::Shader> vertex_shader=new osg::Shader(osg::Shader::VERTEX,texture_vertex);
   program->addShader(vertex_shader.get());

   osg::ref_ptr<osg::Shader> fragment_shader=new osg::Shader(osg::Shader::FRAGMENT,texture_fragment);
   program->addShader(fragment_shader.get());

   stateset->setAttribute(program.get());

   stateset->setTextureAttributeAndModes(0,new osg::Texture2D(imageList.at(tex_num).get()));

   osg::ref_ptr<osg::Uniform> baseMap=new osg::Uniform("baseMap",0);
   stateset->addUniform(baseMap.get());
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::PCF(osg::ref_ptr<osg::Texture2D> shadow,osg::ref_ptr<osg::Node> node)
{
   osg::ref_ptr<osg::StateSet> stateset=node->getOrCreateStateSet();
   osg::ref_ptr<osg::Program> program=new osg::Program;

   osg::ref_ptr<osg::Shader> vertex_shader=new osg::Shader(osg::Shader::VERTEX,pcf_vertex);
   program->addShader(vertex_shader.get());

   osg::ref_ptr<osg::Shader> fragment_shader=new osg::Shader(osg::Shader::FRAGMENT,pcf_fragment);
   program->addShader(fragment_shader.get());

   stateset->setAttribute(program.get());
   
   stateset->setTextureAttributeAndModes(0,shadow.get(),osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_S,osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_T,osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_R,osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_Q,osg::StateAttribute::ON);

   osg::ref_ptr<osg::Uniform> shadowMap=new osg::Uniform("shadowMap",0);
   stateset->addUniform(shadowMap.get());
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::Reflection(float refl_perc,osg::ref_ptr<osg::Node> node)
{
   osg::ref_ptr<osg::StateSet> stateset=node->getOrCreateStateSet();
   osg::ref_ptr<osg::Program> program=new osg::Program;

   osg::ref_ptr<osg::Shader> vertex_shader=new osg::Shader(osg::Shader::VERTEX,reflection_vertex);
   program->addShader(vertex_shader.get());

   osg::ref_ptr<osg::Shader> fragment_shader=new osg::Shader(osg::Shader::FRAGMENT,reflection_fragment);
   program->addShader(fragment_shader.get());

   stateset->setAttribute(program.get());

   stateset->setTextureAttributeAndModes(0,tcm.get(),osg::StateAttribute::ON);

   osg::ref_ptr<osg::Uniform> envMap=new osg::Uniform("envMap",0);
   stateset->addUniform(envMap.get());

   osg::ref_ptr<osg::Uniform> reflection_percentage=new osg::Uniform("refl_perc",refl_perc);
   stateset->addUniform(reflection_percentage.get());
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::XRay(osg::ref_ptr<osg::Node> node)
{
   osg::ref_ptr<osg::StateSet> stateset=node->getOrCreateStateSet();
   osg::ref_ptr<osg::Program> program=new osg::Program;

   osg::ref_ptr<osg::Shader> vertex_shader=new osg::Shader(osg::Shader::VERTEX,xray_vertex);
   program->addShader(vertex_shader.get());

   osg::ref_ptr<osg::Shader> fragment_shader=new osg::Shader(osg::Shader::FRAGMENT,xray_fragment);
   program->addShader(fragment_shader.get());

   stateset->setAttribute(program.get());

   stateset->setMode(GL_BLEND,osg::StateAttribute::ON);
   stateset->setRenderingHint(osg::StateSet::TRANSPARENT_BIN);
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::Phong_Texture(int tex_num,osg::ref_ptr<osg::Node> node)
{
   osg::ref_ptr<osg::StateSet> stateset=node->getOrCreateStateSet();
   osg::ref_ptr<osg::Program> program=new osg::Program;

   osg::ref_ptr<osg::Shader> vertex_shader=new osg::Shader(osg::Shader::VERTEX,phong_texture_vertex);
   program->addShader(vertex_shader.get());

   osg::ref_ptr<osg::Shader> fragment_shader=new osg::Shader(osg::Shader::FRAGMENT,phong_texture_fragment);
   program->addShader(fragment_shader.get());

   stateset->setAttribute(program.get());

   stateset->setTextureAttributeAndModes(0,new osg::Texture2D(imageList.at(tex_num).get()));

   osg::ref_ptr<osg::Uniform> baseMap=new osg::Uniform("baseMap",0);
   stateset->addUniform(baseMap.get());
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::Phong_PCF(osg::ref_ptr<osg::Texture2D> shadow,osg::ref_ptr<osg::Node> node)
{
   osg::ref_ptr<osg::StateSet> stateset=node->getOrCreateStateSet();
   osg::ref_ptr<osg::Program> program=new osg::Program;

   osg::ref_ptr<osg::Shader> vertex_shader=new osg::Shader(osg::Shader::VERTEX,phong_pcf_vertex);
   program->addShader(vertex_shader.get());

   osg::ref_ptr<osg::Shader> fragment_shader=new osg::Shader(osg::Shader::FRAGMENT,phong_pcf_fragment);
   program->addShader(fragment_shader.get());

   stateset->setAttribute(program.get());
   
   stateset->setTextureAttributeAndModes(0,shadow.get(),osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_S,osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_T,osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_R,osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_Q,osg::StateAttribute::ON);

   osg::ref_ptr<osg::Uniform> shadowMap=new osg::Uniform("shadowMap",0);
   stateset->addUniform(shadowMap.get());
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::Phong_Reflection(float refl_perc,osg::ref_ptr<osg::Node> node)
{
   osg::ref_ptr<osg::StateSet> stateset=node->getOrCreateStateSet();
   osg::ref_ptr<osg::Program> program=new osg::Program;

   osg::ref_ptr<osg::Shader> vertex_shader=new osg::Shader(osg::Shader::VERTEX,phong_reflection_vertex);
   program->addShader(vertex_shader.get());

   osg::ref_ptr<osg::Shader> fragment_shader=new osg::Shader(osg::Shader::FRAGMENT,phong_reflection_fragment);
   program->addShader(fragment_shader.get());

   stateset->setAttribute(program.get());

   stateset->setTextureAttributeAndModes(0,tcm.get(),osg::StateAttribute::ON);

   osg::ref_ptr<osg::Uniform> envMap=new osg::Uniform("envMap",0);
   stateset->addUniform(envMap.get());

   osg::ref_ptr<osg::Uniform> reflection_percentage=new osg::Uniform("refl_perc",refl_perc);
   stateset->addUniform(reflection_percentage.get());
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::Texture_PCF(int tex_num,osg::ref_ptr<osg::Texture2D> shadow,osg::ref_ptr<osg::Node> node)
{
   osg::ref_ptr<osg::StateSet> stateset=node->getOrCreateStateSet();
   osg::ref_ptr<osg::Program> program=new osg::Program;

   osg::ref_ptr<osg::Shader> vertex_shader=new osg::Shader(osg::Shader::VERTEX,texture_pcf_vertex);
   program->addShader(vertex_shader.get());

   osg::ref_ptr<osg::Shader> fragment_shader=new osg::Shader(osg::Shader::FRAGMENT,texture_pcf_fragment);
   program->addShader(fragment_shader.get());

   stateset->setAttribute(program.get());
   
   stateset->setTextureAttributeAndModes(0,shadow.get(),osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_S,osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_T,osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_R,osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_Q,osg::StateAttribute::ON);

   stateset->setTextureAttributeAndModes(1,new osg::Texture2D(imageList.at(tex_num).get()));

   osg::ref_ptr<osg::Uniform> shadowMap=new osg::Uniform("shadowMap",0);
   stateset->addUniform(shadowMap.get());

   osg::ref_ptr<osg::Uniform> baseMap=new osg::Uniform("baseMap",1);
   stateset->addUniform(baseMap.get());
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::Texture_Reflection(int tex_num,float refl_perc,osg::ref_ptr<osg::Node> node)
{
   osg::ref_ptr<osg::StateSet> stateset=node->getOrCreateStateSet();
   osg::ref_ptr<osg::Program> program=new osg::Program;

   osg::ref_ptr<osg::Shader> vertex_shader=new osg::Shader(osg::Shader::VERTEX,texture_reflection_vertex);
   program->addShader(vertex_shader.get());

   osg::ref_ptr<osg::Shader> fragment_shader=new osg::Shader(osg::Shader::FRAGMENT,texture_reflection_fragment);
   program->addShader(fragment_shader.get());

   stateset->setAttribute(program.get());

   stateset->setTextureAttributeAndModes(0,tcm.get(),osg::StateAttribute::ON);

   stateset->setTextureAttributeAndModes(1,new osg::Texture2D(imageList.at(tex_num).get()));

   osg::ref_ptr<osg::Uniform> envMap=new osg::Uniform("envMap",0);
   stateset->addUniform(envMap.get());

   osg::ref_ptr<osg::Uniform> baseMap=new osg::Uniform("baseMap",1);
   stateset->addUniform(baseMap.get());

   osg::ref_ptr<osg::Uniform> reflection_percentage=new osg::Uniform("refl_perc",refl_perc);
   stateset->addUniform(reflection_percentage.get());
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::PCF_Reflection(float refl_perc,osg::ref_ptr<osg::Texture2D> shadow,osg::ref_ptr<osg::Node> node)
{
   osg::ref_ptr<osg::StateSet> stateset=node->getOrCreateStateSet();
   osg::ref_ptr<osg::Program> program=new osg::Program;

   osg::ref_ptr<osg::Shader> vertex_shader=new osg::Shader(osg::Shader::VERTEX,pcf_reflection_vertex);
   program->addShader(vertex_shader.get());

   osg::ref_ptr<osg::Shader> fragment_shader=new osg::Shader(osg::Shader::FRAGMENT,pcf_reflection_fragment);
   program->addShader(fragment_shader.get());

   stateset->setAttribute(program.get());
   
   stateset->setTextureAttributeAndModes(0,shadow.get(),osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_S,osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_T,osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_R,osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_Q,osg::StateAttribute::ON);

   stateset->setTextureAttributeAndModes(1,tcm.get(),osg::StateAttribute::ON);

   osg::ref_ptr<osg::Uniform> shadowMap=new osg::Uniform("shadowMap",0);
   stateset->addUniform(shadowMap.get());

   osg::ref_ptr<osg::Uniform> envMap=new osg::Uniform("envMap",1);
   stateset->addUniform(envMap.get());

   osg::ref_ptr<osg::Uniform> reflection_percentage=new osg::Uniform("refl_perc",refl_perc);
   stateset->addUniform(reflection_percentage.get());
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::Phong_Texture_PCF(int tex_num,osg::ref_ptr<osg::Texture2D> shadow,osg::ref_ptr<osg::Node> node)
{
   osg::ref_ptr<osg::StateSet> stateset=node->getOrCreateStateSet();
   osg::ref_ptr<osg::Program> program=new osg::Program;

   osg::ref_ptr<osg::Shader> vertex_shader=new osg::Shader(osg::Shader::VERTEX,phong_texture_pcf_vertex);
   program->addShader(vertex_shader.get());

   osg::ref_ptr<osg::Shader> fragment_shader=new osg::Shader(osg::Shader::FRAGMENT,phong_texture_pcf_fragment);
   program->addShader(fragment_shader.get());

   stateset->setAttribute(program.get());
   
   stateset->setTextureAttributeAndModes(0,shadow.get(),osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_S,osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_T,osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_R,osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_Q,osg::StateAttribute::ON);

   stateset->setTextureAttributeAndModes(1,new osg::Texture2D(imageList.at(tex_num).get()));

   osg::ref_ptr<osg::Uniform> shadowMap=new osg::Uniform("shadowMap",0);
   stateset->addUniform(shadowMap.get());

   osg::ref_ptr<osg::Uniform> baseMap=new osg::Uniform("baseMap",1);
   stateset->addUniform(baseMap.get());
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::Phong_Texture_Reflection(int tex_num,float refl_perc,osg::ref_ptr<osg::Node> node)
{
   osg::ref_ptr<osg::StateSet> stateset=node->getOrCreateStateSet();
   osg::ref_ptr<osg::Program> program=new osg::Program;

   osg::ref_ptr<osg::Shader> vertex_shader=new osg::Shader(osg::Shader::VERTEX,phong_texture_reflection_vertex);
   program->addShader(vertex_shader.get());

   osg::ref_ptr<osg::Shader> fragment_shader=new osg::Shader(osg::Shader::FRAGMENT,phong_texture_reflection_fragment);
   program->addShader(fragment_shader.get());

   stateset->setAttribute(program.get());

   stateset->setTextureAttributeAndModes(0,tcm.get(),osg::StateAttribute::ON);

   stateset->setTextureAttributeAndModes(1,new osg::Texture2D(imageList.at(tex_num).get()));

   osg::ref_ptr<osg::Uniform> envMap=new osg::Uniform("envMap",0);
   stateset->addUniform(envMap.get());

   osg::ref_ptr<osg::Uniform> baseMap=new osg::Uniform("baseMap",1);
   stateset->addUniform(baseMap.get());

   osg::ref_ptr<osg::Uniform> reflection_percentage=new osg::Uniform("refl_perc",refl_perc);
   stateset->addUniform(reflection_percentage.get());
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::Phong_PCF_Reflection(float refl_perc,osg::ref_ptr<osg::Texture2D> shadow,osg::ref_ptr<osg::Node> node)
{
   osg::ref_ptr<osg::StateSet> stateset=node->getOrCreateStateSet();
   osg::ref_ptr<osg::Program> program=new osg::Program;

   osg::ref_ptr<osg::Shader> vertex_shader=new osg::Shader(osg::Shader::VERTEX,phong_pcf_reflection_vertex);
   program->addShader(vertex_shader.get());

   osg::ref_ptr<osg::Shader> fragment_shader=new osg::Shader(osg::Shader::FRAGMENT,phong_pcf_reflection_fragment);
   program->addShader(fragment_shader.get());

   stateset->setAttribute(program.get());

   stateset->setTextureAttributeAndModes(0,shadow.get(),osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_S,osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_T,osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_R,osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_Q,osg::StateAttribute::ON);

   stateset->setTextureAttributeAndModes(1,tcm.get(),osg::StateAttribute::ON);

   osg::ref_ptr<osg::Uniform> shadowMap=new osg::Uniform("shadowMap",0);
   stateset->addUniform(shadowMap.get());

   osg::ref_ptr<osg::Uniform> envMap=new osg::Uniform("envMap",1);
   stateset->addUniform(envMap.get());

   osg::ref_ptr<osg::Uniform> reflection_percentage=new osg::Uniform("refl_perc",refl_perc);
   stateset->addUniform(reflection_percentage.get());
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::Texture_PCF_Reflection(int tex_num,float refl_perc,osg::ref_ptr<osg::Texture2D> shadow,osg::ref_ptr<osg::Node> node)
{
   osg::ref_ptr<osg::StateSet> stateset=node->getOrCreateStateSet();
   osg::ref_ptr<osg::Program> program=new osg::Program;

   osg::ref_ptr<osg::Shader> vertex_shader=new osg::Shader(osg::Shader::VERTEX,texture_pcf_reflection_vertex);
   program->addShader(vertex_shader.get());

   osg::ref_ptr<osg::Shader> fragment_shader=new osg::Shader(osg::Shader::FRAGMENT,texture_pcf_reflection_fragment);
   program->addShader(fragment_shader.get());

   stateset->setAttribute(program.get());

   stateset->setTextureAttributeAndModes(0,shadow.get(),osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_S,osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_T,osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_R,osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_Q,osg::StateAttribute::ON);

   stateset->setTextureAttributeAndModes(1,tcm.get(),osg::StateAttribute::ON);

   stateset->setTextureAttributeAndModes(2,new osg::Texture2D(imageList.at(tex_num).get()));

   osg::ref_ptr<osg::Uniform> shadowMap=new osg::Uniform("shadowMap",0);
   stateset->addUniform(shadowMap.get());

   osg::ref_ptr<osg::Uniform> envMap=new osg::Uniform("envMap",1);
   stateset->addUniform(envMap.get());

   osg::ref_ptr<osg::Uniform> baseMap=new osg::Uniform("baseMap",2);
   stateset->addUniform(baseMap.get());

   osg::ref_ptr<osg::Uniform> reflection_percentage=new osg::Uniform("refl_perc",refl_perc);
   stateset->addUniform(reflection_percentage.get());
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::Phong_Texture_PCF_Reflection(int tex_num,float refl_perc,osg::ref_ptr<osg::Texture2D> shadow,osg::ref_ptr<osg::Node> node)
{
   osg::ref_ptr<osg::StateSet> stateset=node->getOrCreateStateSet();
   osg::ref_ptr<osg::Program> program=new osg::Program;

   osg::ref_ptr<osg::Shader> vertex_shader=new osg::Shader(osg::Shader::VERTEX,phong_texture_pcf_reflection_vertex);
   program->addShader(vertex_shader.get());

   osg::ref_ptr<osg::Shader> fragment_shader=new osg::Shader(osg::Shader::FRAGMENT,phong_texture_pcf_reflection_fragment);
   program->addShader(fragment_shader.get());

   stateset->setAttribute(program.get());

   stateset->setTextureAttributeAndModes(0,shadow.get(),osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_S,osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_T,osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_R,osg::StateAttribute::ON);
   stateset->setTextureMode(0,GL_TEXTURE_GEN_Q,osg::StateAttribute::ON);

   stateset->setTextureAttributeAndModes(1,tcm.get(),osg::StateAttribute::ON);

   stateset->setTextureAttributeAndModes(2,new osg::Texture2D(imageList.at(tex_num).get()));

   osg::ref_ptr<osg::Uniform> shadowMap=new osg::Uniform("shadowMap",0);
   stateset->addUniform(shadowMap.get());

   osg::ref_ptr<osg::Uniform> envMap=new osg::Uniform("envMap",1);
   stateset->addUniform(envMap.get());

   osg::ref_ptr<osg::Uniform> baseMap=new osg::Uniform("baseMap",2);
   stateset->addUniform(baseMap.get());

   osg::ref_ptr<osg::Uniform> reflection_percentage=new osg::Uniform("refl_perc",refl_perc);
   stateset->addUniform(reflection_percentage.get());
}
////////////////////////////////////////////////////////////////////////////////
