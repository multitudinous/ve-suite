// --- My Includes --- //
#include "Shaders.h"
#include "VertFrag.h"

// --- OSG Includes --- //
#include <osg/StateSet>

////////////////////////////////////////////////////////////////////////////////
Shaders::Shaders()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Shaders::~Shaders()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
osg::ref_ptr< osg::StateSet > Shaders::Phong()
{
    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
    osg::ref_ptr< osg::Program > program = new osg::Program();

    stateset->setAttribute( program.get(), osg::StateAttribute::ON | osg::StateAttribute::OVERRIDE );

    osg::ref_ptr< osg::Shader > vertex_shader = new osg::Shader( osg::Shader::VERTEX, phongVertex );
    osg::ref_ptr< osg::Shader > fragment_shader = new osg::Shader( osg::Shader::FRAGMENT, phongFragment );
    program->addShader( vertex_shader.get() );
    program->addShader( fragment_shader.get() );

    osg::ref_ptr< osg::Uniform > ambientMaterial = new osg::Uniform(
        "ambientMaterial", osg::Vec3( 0.368627, 0.368421, 0.368421 ) );
    stateset->addUniform( ambientMaterial.get() );

    osg::ref_ptr< osg::Uniform > diffuseMaterial = new osg::Uniform(
        "diffuseMaterial", osg::Vec3( 0.886275, 0.885003, 0.885003 ) );
    stateset->addUniform( diffuseMaterial.get() );

    osg::ref_ptr< osg::Uniform > specularMaterial = new osg::Uniform(
        "specularMaterial", osg::Vec3( 0.490196, 0.488722, 0.488722 ) );
    stateset->addUniform( specularMaterial.get() );

    float specularPowerValue = 20.0;
    osg::ref_ptr< osg::Uniform > specularPower = new osg::Uniform(
        "specularPower", specularPowerValue );
    stateset->addUniform( specularPower.get() );

    return stateset.get();
}
////////////////////////////////////////////////////////////////////////////////
osg::ref_ptr<osg::StateSet> Shaders::XRay()
{
    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
    osg::ref_ptr< osg::Program > program = new osg::Program();

    stateset->setAttribute( program.get() );
    stateset->setMode( GL_BLEND, osg::StateAttribute::ON );
    stateset->setRenderingHint( osg::StateSet::TRANSPARENT_BIN );

    osg::ref_ptr< osg::Shader > vertex_shader = new osg::Shader( osg::Shader::VERTEX, xrayVertex );
    osg::ref_ptr< osg::Shader > fragment_shader = new osg::Shader( osg::Shader::FRAGMENT, xrayFragment );
    program->addShader( vertex_shader.get() );
    program->addShader( fragment_shader.get() );

    return stateset.get();
}
////////////////////////////////////////////////////////////////////////////////
