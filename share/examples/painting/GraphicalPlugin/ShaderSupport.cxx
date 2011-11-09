// Name: VirtualPaint Demo
// ShaderSupport.cpp
//
// Authors:
//   Chris 'Xenon' Hanson, AlphaPixel.
//   John Enright, Digital Transforms.
//
// Origination Date: July 19, 2011

// STL includes
#include <string>
#include <iostream>
#include <sstream>
#include <osgText/Text>

#include "ShaderSupport.h"

#include <osg/Program>
#include <osg/Shader>
#include <osg/Uniform>
#include <osgDB/FileUtils>

// a convenience so don't have to prefix STL stuff with "std::".
using namespace std;
// ditto osg
using namespace osg;

// snippets from OSG's osgshaders example, GL2Scene.cpp

osg::ref_ptr<osg::Uniform> uni_strength;
osg::ref_ptr<osg::Uniform> uni_TargetMin;
osg::ref_ptr<osg::Uniform> uni_TargetMax;

// load source from a file.
void LoadShaderSource( osg::Shader* shader, const std::string& fileName )
{
	std::string fqFileName = osgDB::findDataFile(fileName);
	if( fqFileName.length() != 0 )
	{
		shader->loadShaderSourceFromFile( fqFileName.c_str() );
	}
	else
	{
		osg::notify(osg::WARN) << "Shader file \"" << fileName << "\" not found." << std::endl;
	}
}

///////////////////////////////////////////////////////////////////////////
// rude but convenient globals

bool accumEnabled(false);
osg::ref_ptr<osg::Uniform> uni_AccumEnabled1;


void toggleAccumEnabled(void)
{
	accumEnabled = !accumEnabled;
	uni_AccumEnabled1->set( (accumEnabled ? 1.0f : 0.0f) );
} // toggleAccumEnabled

void setupShaders(osg::Node* background, osg::Node* model)
{
	osg::Program* AccumProgram;
	osg::Shader*  AccumVertObj;
	osg::Shader*  AccumFragObj;

	AccumProgram = new osg::Program;
	AccumProgram->setName( "accum" );
	AccumVertObj = new osg::Shader( osg::Shader::VERTEX );
	AccumFragObj = new osg::Shader( osg::Shader::FRAGMENT );
// 	LoadShaderSource( AccumVertObj, "shaders/accumVertex.glsl" );
// 	LoadShaderSource( AccumFragObj, "shaders/accum.glsl" );
	LoadShaderSource( AccumVertObj, "shaders/AccumGLSL.vert" );
	LoadShaderSource( AccumFragObj, "shaders/AccumGLSL.frag" );
	AccumProgram->addShader( AccumFragObj );
	AccumProgram->addShader( AccumVertObj );
	AccumProgram->addBindAttribLocation( "rm_Tangent", TANGENT_ATR_UNIT );
	AccumProgram->addBindAttribLocation( "rm_Binormal", BINORMAL_ATR_UNIT );

	background->getOrCreateStateSet()->setAttributeAndModes(AccumProgram, osg::StateAttribute::ON);
	if(model)
	{
		model->getOrCreateStateSet()->setAttributeAndModes(AccumProgram, osg::StateAttribute::ON);
	} // if
	// setup uniforms

/*
	uniform sampler2DRect _texture1; <<<>>>
*/
	uni_AccumEnabled1       = new osg::Uniform( "_AccumEnabled1", 0.0f );
	osg::Uniform* uni_passthrough1        = new osg::Uniform( "_passthrough1", 0.0f );
	osg::Uniform* uni_distance1           = new osg::Uniform( "_distance1", 30.48f ); // 12 inches in CM
	osg::Uniform* uni_focaldistance1      = new osg::Uniform( "_focaldistance1", 25.4f ); // 10 inches in CM
	osg::Uniform* uni_targetThickness1    = new osg::Uniform( "_targetThickness1", 5.0f ); // default: 1 mils
	osg::Uniform* uni_targetThicknessMax1 = new osg::Uniform( "_targetThicknessMax1", 50.0f ); // default: 2 mils
	osg::Uniform* uni_laserEnabled1       = new osg::Uniform( "_laserEnabled1", 0.0f );
	osg::Uniform* uni_dotScale1           = new osg::Uniform( "_dotScale1", 0.0f );
	osg::Uniform* uni_roll1               = new osg::Uniform( "_roll1", 0.0f );
	osg::Uniform* uni_texelAreaCM1        = new osg::Uniform( "_texelAreaCM1", 1.0f ); // 1cm^2
	osg::Uniform* uni_texSize1            = new osg::Uniform( "_texSize1", 1.0f ); // appears to be pixel dimension of NPOT, texture. POT uses 1.0
	osg::Uniform* uni_gloss1              = new osg::Uniform( "_gloss1", 44.0f );
	osg::Uniform* uni_refl1               = new osg::Uniform( "_refl1", 0.16f );
	osg::Uniform* uni_normalBias1         = new osg::Uniform( "_normalBias1", 0.25f );
	osg::Uniform* uni_max_reflection1     = new osg::Uniform( "_max_reflection1", 1.0f );

	osg::Uniform* uni_intPt1              = new osg::Uniform( "_intPt1", osg::Vec2(5.0f, 5.0f) );
	osg::Uniform* uni_surfaceColor1       = new osg::Uniform( "_surfaceColor1", osg::Vec3(0.95f, 0.74f, 0.0f) ); // target: (0.95f, 0.74f, 0.0f), background: (1.0, 1.0, 1.0)

	uni_TargetMin           = new osg::Uniform( "TargetMin", 100.0f );
	uni_TargetMax           = new osg::Uniform( "TargetMax", 230.f );

	// new uniforms for Complex surfaces shader
	// <<<>>> this will need to be applied to model as well as background
	osg::StateSet* ss = background->getOrCreateStateSet();
	// noise/permtexture not currently used
	//ss->setTextureAttribute(TEXUNIT_PERM, PermTexture);
	ss->addUniform( new osg::Uniform("permTexture", TEXUNIT_PERM) );

	ss->addUniform( new osg::Uniform("bgTexture", TEXUNIT_BG) );
	ss->addUniform( new osg::Uniform("accumTexture", TEXUNIT_ACCUM) );

	ss->addUniform( new osg::Uniform("fSpecularPower", 25.0f) );
	ss->addUniform( new osg::Uniform("fScale", 1.0f) );

	ss->addUniform( new osg::Uniform("fvLightPosition", osg::Vec3(100.0f, 100.0f, 100.0f)) );
	ss->addUniform( new osg::Uniform("fvEyePosition", osg::Vec3(100.0f, 100.0f, 100.0f)) );

	ss->addUniform( new osg::Uniform("fvDiffuse", osg::Vec4(0.8863f, 0.8850f, 0.8850f, 1.0f)) );
	ss->addUniform( new osg::Uniform("fvSpecular", osg::Vec4(0.57f, 0.57f, 0.57f, 1.0f)) );
	ss->addUniform( new osg::Uniform("fvAmbient", osg::Vec4(0.3686f, 0.3686f, 0.3686f, 1.0f)) );

	ss->addUniform( uni_AccumEnabled1.get() );
	ss->addUniform( uni_passthrough1 );
	ss->addUniform( uni_distance1 );
	ss->addUniform( uni_focaldistance1 );
	ss->addUniform( uni_targetThickness1 );
	ss->addUniform( uni_targetThicknessMax1 );
	ss->addUniform( uni_laserEnabled1 );
	ss->addUniform( uni_dotScale1 );
	ss->addUniform( uni_roll1 );
	ss->addUniform( uni_texelAreaCM1 );
	ss->addUniform( uni_texSize1 );
	ss->addUniform( uni_gloss1 );
	ss->addUniform( uni_refl1 );
	ss->addUniform( uni_normalBias1 );
	ss->addUniform( uni_max_reflection1 );

	ss->addUniform( uni_intPt1 );
	ss->addUniform( uni_surfaceColor1 );

	ss->addUniform( uni_TargetMin.get() );
	ss->addUniform( uni_TargetMax.get() );

} // setupShaders

void setupSpatterShaders(osg::Node* spatterGraph)
{
	osg::Program* SpatterProgram;
	osg::Shader*  SpatterVertObj;
	osg::Shader*  SpatterFragObj;

	SpatterProgram = new osg::Program;
	SpatterProgram->setName( "Spatter" );
	SpatterVertObj = new osg::Shader( osg::Shader::VERTEX );
	SpatterFragObj = new osg::Shader( osg::Shader::FRAGMENT );
	LoadShaderSource( SpatterVertObj, "shaders/AirlessSpatter.vert" );
	LoadShaderSource( SpatterFragObj, "shaders/AirlessSpatter.frag" );
	SpatterProgram->addShader( SpatterFragObj );
	SpatterProgram->addShader( SpatterVertObj );

	spatterGraph->getOrCreateStateSet()->setAttributeAndModes(SpatterProgram, osg::StateAttribute::ON);

    // setup uniforms
	uni_strength       = new osg::Uniform( "strength", 1.0f );
	osg::StateSet* ss = spatterGraph->getOrCreateStateSet();
	ss->addUniform( new osg::Uniform("spatterTexture", TEXUNIT_SPATTER) );
	ss->addUniform( uni_strength.get() );
} // setupSpatterShaders

void setSpatterStrength(float newStrength)
{
    uni_strength->set( newStrength );
}