// --- My Includes --- //
#include "Shaders.h"
#include "VertFrag.h"

// --- OSG Includes --- //
#include <osg/Node>
#include <osg/StateSet>

////////////////////////////////////////////////////////////////////////////////
Shaders::Shaders()
:
m_phong( new osg::StateSet() ),
m_xray( new osg::StateSet() )
{
    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
Shaders::~Shaders()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::Initialize()
{
    osg::ref_ptr< osg::Program > phongProgram = new osg::Program();
    osg::ref_ptr< osg::Program > xrayProgram = new osg::Program();

    m_phong->setAttribute( phongProgram.get(), osg::StateAttribute::ON );
    m_xray->setAttribute( xrayProgram.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Shader > phongVertexShader =
        new osg::Shader( osg::Shader::VERTEX, phongVertex );
    osg::ref_ptr< osg::Shader > phongFragmentShader =
        new osg::Shader( osg::Shader::FRAGMENT, phongFragment );
    phongProgram->addShader( phongVertexShader.get() );
    phongProgram->addShader( phongFragmentShader.get() );

    osg::ref_ptr< osg::Shader > xrayVertexShader =
        new osg::Shader( osg::Shader::VERTEX, xrayVertex );
    osg::ref_ptr< osg::Shader > xrayFragmentShader =
        new osg::Shader( osg::Shader::FRAGMENT, xrayFragment );
    xrayProgram->addShader( xrayVertexShader.get() );
    xrayProgram->addShader( xrayFragmentShader.get() );

    osg::ref_ptr< osg::Uniform > ambientMaterial = new osg::Uniform(
        "ambientMaterial", osg::Vec3( 0.368627, 0.368421, 0.368421 ) );
    m_phong->addUniform( ambientMaterial.get() );

    osg::ref_ptr< osg::Uniform > diffuseMaterial = new osg::Uniform(
        "diffuseMaterial", osg::Vec3( 0.886275, 0.885003, 0.885003 ) );
    m_phong->addUniform( diffuseMaterial.get() );

    osg::ref_ptr< osg::Uniform > specularMaterial = new osg::Uniform(
        "specularMaterial", osg::Vec3( 0.490196, 0.488722, 0.488722 ) );
    m_phong->addUniform( specularMaterial.get() );

    float specularPowerValue = 20.0;
    osg::ref_ptr< osg::Uniform > specularPower = new osg::Uniform(
        "specularPower", specularPowerValue );
    m_phong->addUniform( specularPower.get() );

    m_phong->setRenderBinDetails( 10, std::string( "DepthSortedBin" ) );

    m_xray->setMode( GL_BLEND, osg::StateAttribute::ON );
    m_xray->setRenderBinDetails( 0, std::string( "RenderBin" ) );
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::Phong( osg::ref_ptr< osg::Node > node )
{
    node->setStateSet( m_phong.get() );
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::XRay( osg::ref_ptr< osg::Node > node )
{
    node->setStateSet( m_xray.get() );
}
////////////////////////////////////////////////////////////////////////////////
