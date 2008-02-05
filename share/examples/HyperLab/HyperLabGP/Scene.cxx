// --- VE-Suite Includes --- //
#include "Shaders.h"
#include "Scene.h"

// --- OSG Includes --- //
#include <osg/Projection>
#include <osg/AnimationPath>
#include <osg/Texture2D>
#include <osg/Texture3D>
#include <osg/Group>
#include <osg/Geode>
#include <osg/ShapeDrawable>
#include <osg/MatrixTransform>
#include <osg/Light>
#include <osg/LightSource>
#include <osg/PolygonOffset>
#include <osg/Material>
#include <osg/Camera>
#include <osg/TexGenNode>
#include <osg/CullFace>

#include <osgDB/ReadFile>
#include <osgDB/WriteFile>

// --- C/C++ Libraries --- //
#include <iostream>
#include <sstream>

////////////////////////////////////////////////////////////////////////////////
Scene::Scene()
:
shader( new Shaders() ),

root( new osg::Group() ),
shadowed_scene( new osg::Group() ),
non_shadowed_scene( new osg::Group() ),

light_1( new osg::Light() ),
light_source_1( new osg::LightSource() ),
light_transform_1( new osg::MatrixTransform() ),

shadow( new osg::Texture2D() ),
jitter( new osg::Texture3D() ),
camera( new osg::Camera() ),
texgenNode( new osg::TexGenNode() ),

coronas( new osg::Group() ),

aluminum_pipes_material( new osg::Material() ),
aluminum_parts_material( new osg::Material() ),
black_material( new osg::Material() ),
brown_material( new osg::Material() ),
details_material( new osg::Material() ),
glass_material( new osg::Material() ),
lights_material( new osg::Material() ),
lt_green_material( new osg::Material() ),
lt_grey_material( new osg::Material() ),
orange_material( new osg::Material() ),
red_material( new osg::Material() ),
red_brown_material( new osg::Material() ),
ceiling_material( new osg::Material() ),
floor_material( new osg::Material() ),
walls_material( new osg::Material() ),
white_pipes_material( new osg::Material() ),
white_ducts_material( new osg::Material() ),
yellow_material( new osg::Material() ),

frame_material( new osg::Material() ),
railing_material( new osg::Material() ),
plenum_piping_material( new osg::Material() ),
blower_components_material( new osg::Material() ),
brackets_material( new osg::Material() ),
cement_base_material( new osg::Material() ),
combustor_piping_material( new osg::Material() ),
compressor_inlet_material( new osg::Material() ),
heat_exchanger_material( new osg::Material() ),
heat_exchanger_sweep_material( new osg::Material() ),
load_material( new osg::Material() ),
plenum_system_material( new osg::Material() ),
relief_piping_material( new osg::Material() ),
shell_material( new osg::Material() ),
stack_material( new osg::Material() ),
turbine_exhaust_material( new osg::Material() ),
turbine_postcombustor_material( new osg::Material() ),
miscellaneous_material( new osg::Material() )
{
    CreateLights();
    CreateNodes();
    CreateShadowTexture();
    //CreateJitterTexture();

    //WriteOutShadow();

    Defaults();
}
////////////////////////////////////////////////////////////////////////////////
Scene::~Scene()
{
    if( shader )
    {
        delete shader;
    }
}
////////////////////////////////////////////////////////////////////////////////
osg::ref_ptr< osg::Group > Scene::InitScene()
{
    root->addChild( shadowed_scene.get() );
    root->addChild( non_shadowed_scene.get() );
    root->addChild( light_transform_1.get() );
    root->addChild( coronas.get() );
    root->addChild( camera.get() );
    root->addChild( texgenNode.get() );

    root->getOrCreateStateSet()->setMode( GL_LIGHTING, osg::StateAttribute::ON );

    return root.get();
}
////////////////////////////////////////////////////////////////////////////////
void Scene::CreateLights()
{
    light_1->setLightNum( 1 );
    light_1->setPosition( osg::Vec4( 0.0f, 0.0f, 10000.0f, 0.0f ) );

    light_source_1->setLight( light_1.get() );
    light_source_1->setLocalStateSetModes( osg::StateAttribute::ON );

    light_transform_1->setMatrix( osg::Matrix::translate( osg::Vec3( 0.0f, 0.0f, 10000.0f ) ) );
    light_transform_1->addChild( light_source_1.get() );

    /*
    osg::ref_ptr< osg::Geode > geode = new osg::Geode();

    osg::ref_ptr< osg::TessellationHints > hints = new osg::TessellationHints();
    hints->setDetailRatio( 4.0f );
    osg::ref_ptr< osg::ShapeDrawable > shape = new osg::ShapeDrawable( new osg::Sphere( light_transform_1->getMatrix().getTrans(), 50.0f ), hints.get() );
    shape->setColor( osg::Vec4( 1.0f, 1.0f, 0.0f, 1.0f ) );
    geode->addDrawable( shape.get() );

    light_transform_1->addChild( geode.get() );
    */
}
////////////////////////////////////////////////////////////////////////////////
void Scene::CreateNodes()
{
    osg::ref_ptr< osg::StateSet> stateset = new osg::StateSet();

    //Load in the coronas :)
    stateset=coronas->getOrCreateStateSet();
    for( int i = 0; i < 39; ++i )
    {
        std::stringstream name;
        name << "./IVEs/Room/Corona" << i << ".ive";

        coronas->addChild( osgDB::readNodeFile( name.str() ) );
    }

    //Load in the shadowed group
    aluminum_parts=osgDB::readNodeFile("./IVEs/Room/AluminumParts.ive");
    stateset=aluminum_parts->getOrCreateStateSet();
    stateset->setAttributeAndModes(aluminum_parts_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(aluminum_parts.get());

    aluminum_pipes=osgDB::readNodeFile("./IVEs/Room/AluminumPipes.ive");
    stateset=aluminum_pipes->getOrCreateStateSet();
    stateset->setAttributeAndModes(aluminum_pipes_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(aluminum_pipes.get());

    black=osgDB::readNodeFile("./IVEs/Room/Black.ive");
    stateset=black->getOrCreateStateSet();
    stateset->setAttributeAndModes(black_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(black.get());

    brown=osgDB::readNodeFile("./IVEs/Room/Brown.ive");
    stateset=brown->getOrCreateStateSet();
    stateset->setAttributeAndModes(brown_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(brown.get());

    details=osgDB::readNodeFile("./IVEs/Room/Details.ive");
    stateset=details->getOrCreateStateSet();
    stateset->setAttributeAndModes(details_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(details.get());

    floor=osgDB::readNodeFile("./IVEs/Room/Floor.ive");
    stateset=floor->getOrCreateStateSet();
    stateset->setAttributeAndModes(floor_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(floor.get());

    lt_green=osgDB::readNodeFile("./IVEs/Room/LtGreen.ive");
    stateset=lt_green->getOrCreateStateSet();
    stateset->setAttributeAndModes(lt_green_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(lt_green.get());

    lt_grey=osgDB::readNodeFile("./IVEs/Room/LtGrey.ive");
    stateset=lt_grey->getOrCreateStateSet();
    stateset->setAttributeAndModes(lt_grey_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(lt_grey.get());

    orange=osgDB::readNodeFile("./IVEs/Room/Orange.ive");
    stateset=orange->getOrCreateStateSet();
    stateset->setAttributeAndModes(orange_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(orange.get());

    red=osgDB::readNodeFile("./IVEs/Room/Red.ive");
    stateset=red->getOrCreateStateSet();
    stateset->setAttributeAndModes(red_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(red.get());

    red_brown=osgDB::readNodeFile("./IVEs/Room/RedBrown.ive");
    stateset=red_brown->getOrCreateStateSet();
    stateset->setAttributeAndModes(red_brown_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(red_brown.get());

    walls=osgDB::readNodeFile("./IVEs/Room/Walls.ive");
    stateset=walls->getOrCreateStateSet();
    stateset->setAttributeAndModes(walls_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(walls.get());

    white_pipes=osgDB::readNodeFile("./IVEs/Room/WhitePipes.ive");
    stateset=white_pipes->getOrCreateStateSet();
    stateset->setAttributeAndModes(white_pipes_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(white_pipes.get());

    white_ducts=osgDB::readNodeFile("./IVEs/Room/WhiteDucts.ive");
    stateset=white_ducts->getOrCreateStateSet();
    stateset->setAttributeAndModes(white_ducts_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(white_ducts.get());

    yellow=osgDB::readNodeFile("./IVEs/Room/Yellow.ive");
    stateset=yellow->getOrCreateStateSet();
    stateset->setAttributeAndModes(yellow_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(yellow.get());

    frame=osgDB::readNodeFile("./IVEs/Frame.ive");
    stateset=frame->getOrCreateStateSet();
    stateset->setAttributeAndModes(frame_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(frame.get());

    railing=osgDB::readNodeFile("./IVEs/Railing.ive");
    stateset=railing->getOrCreateStateSet();
    stateset->setAttributeAndModes(railing_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(railing.get());

    plenum_piping=osgDB::readNodeFile("./IVEs/PlenumPiping.ive");
    stateset=plenum_piping->getOrCreateStateSet();
    stateset->setAttributeAndModes(plenum_piping_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(plenum_piping.get());

    blower_components=osgDB::readNodeFile("./IVEs/BlowerComponents.ive");
    stateset=blower_components->getOrCreateStateSet();
    stateset->setAttributeAndModes(blower_components_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(blower_components.get());

    brackets=osgDB::readNodeFile("./IVEs/Brackets.ive");
    stateset=brackets->getOrCreateStateSet();
    stateset->setAttributeAndModes(plenum_piping_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(brackets.get());

    cement_base=osgDB::readNodeFile("./IVEs/CementBase.ive");
    stateset=cement_base->getOrCreateStateSet();
    stateset->setAttributeAndModes(brackets_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(cement_base.get());

    combustor_piping=osgDB::readNodeFile("./IVEs/CombustorPiping.ive");
    stateset=combustor_piping->getOrCreateStateSet();
    stateset->setAttributeAndModes(combustor_piping_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(combustor_piping.get());

    compressor_inlet=osgDB::readNodeFile("./IVEs/CompressorInlet.ive");
    stateset=compressor_inlet->getOrCreateStateSet();
    stateset->setAttributeAndModes(compressor_inlet_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(compressor_inlet.get());

    heat_exchanger=osgDB::readNodeFile("./IVEs/HeatExchanger.ive");
    stateset=heat_exchanger->getOrCreateStateSet();
    stateset->setAttributeAndModes(heat_exchanger_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(heat_exchanger.get());

    heat_exchanger_sweep=osgDB::readNodeFile("./IVEs/HeatExchangerSweep.ive");
    stateset=heat_exchanger_sweep->getOrCreateStateSet();
    stateset->setAttributeAndModes(heat_exchanger_sweep_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(heat_exchanger_sweep.get());

    load=osgDB::readNodeFile("./IVEs/Load.ive");
    stateset=load->getOrCreateStateSet();
    stateset->setAttributeAndModes(load_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(load.get());

    plenum_system=osgDB::readNodeFile("./IVEs/PlenumSystem.ive");
    stateset=plenum_system->getOrCreateStateSet();
    stateset->setAttributeAndModes(plenum_system_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(plenum_system.get());

    relief_piping=osgDB::readNodeFile("./IVEs/ReliefPiping.ive");
    stateset=relief_piping->getOrCreateStateSet();
    stateset->setAttributeAndModes(relief_piping_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(relief_piping.get());

    shell=osgDB::readNodeFile("./IVEs/Shell.ive");
    stateset=shell->getOrCreateStateSet();
    stateset->setAttributeAndModes(shell_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(shell.get());

    stack=osgDB::readNodeFile("./IVEs/Stack.ive");
    stateset=stack->getOrCreateStateSet();
    stateset->setAttributeAndModes(stack_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(stack.get());

    turbine_exhaust=osgDB::readNodeFile("./IVEs/TurbineExhaust.ive");
    stateset=turbine_exhaust->getOrCreateStateSet();
    stateset->setAttributeAndModes(turbine_exhaust_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(turbine_exhaust.get());

    turbine_postcombustor=osgDB::readNodeFile("./IVEs/TurbinePostCombustor.ive");
    stateset=turbine_postcombustor->getOrCreateStateSet();
    stateset->setAttributeAndModes(turbine_postcombustor_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(turbine_postcombustor.get());

    miscellaneous=osgDB::readNodeFile("./IVEs/Instrumentation.ive");
    stateset=miscellaneous->getOrCreateStateSet();
    stateset->setAttributeAndModes(miscellaneous_material.get(),osg::StateAttribute::ON);
    shadowed_scene->addChild(miscellaneous.get());

    //Load in the non-shadowed group
    ceiling=osgDB::readNodeFile("./IVEs/Room/Ceiling.ive");
    stateset=ceiling->getOrCreateStateSet();
    stateset->setAttributeAndModes(ceiling_material.get(),osg::StateAttribute::ON);
    non_shadowed_scene->addChild(ceiling.get());

    glass=osgDB::readNodeFile("./IVEs/Room/Glass.ive");
    stateset=glass->getOrCreateStateSet();
    stateset->setAttributeAndModes(glass_material.get(),osg::StateAttribute::ON);
    non_shadowed_scene->addChild(glass.get());

    lights=osgDB::readNodeFile("./IVEs/Room/Lights.ive");
    stateset=lights->getOrCreateStateSet();
    stateset->setAttributeAndModes(lights_material.get(),osg::StateAttribute::ON);
    non_shadowed_scene->addChild(lights.get());
}
////////////////////////////////////////////////////////////////////////////////
void Scene::Defaults()
{
    //Set light defaults
    light_1->setAmbient( osg::Vec4( 0.4f, 0.4f, 0.4f, 1.0f ) );
    light_1->setDiffuse( osg::Vec4( 0.9f, 0.9f, 0.9f, 1.0f ) );
    light_1->setSpecular( osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );

    //Set material defaults
    aluminum_parts_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    aluminum_parts_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.4f,0.4f,0.4f,1.0f));
    aluminum_parts_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.4f,0.4f,0.6f,1.0f));
    aluminum_parts_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    aluminum_parts_material->setShininess(osg::Material::FRONT,5.0f);

    aluminum_pipes_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    aluminum_pipes_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.4f,0.4f,0.4f,1.0f));
    aluminum_pipes_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.4f,0.4f,0.6f,1.0f));
    aluminum_pipes_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    aluminum_pipes_material->setShininess(osg::Material::FRONT,5.0f);

    black_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    black_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.0f,0.0f,0.0f,1.0f));
    black_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.0f,0.0f,0.0f,1.0f));
    black_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    black_material->setShininess(osg::Material::FRONT,15.0f);

    brown_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    brown_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.2f,0.1f,0.05f,1.0f));
    brown_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.5f,0.3f,0.15f,1.0f));
    brown_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    brown_material->setShininess(osg::Material::FRONT,15.0f);

    ceiling_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    ceiling_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.3f,0.3f,0.3f,1.0f));
    ceiling_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.3f,0.3f,0.3f,1.0f));
    ceiling_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.1f,0.1f,0.1f,1.0f));
    ceiling_material->setShininess(osg::Material::FRONT,15.0f);

    details_material->setEmission(osg::Material::FRONT,osg::Vec4(2.0f,2.0f,2.0f,1.0f));
    details_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.8f,0.8f,0.8f,1.0f));
    details_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.1f,0.1f,0.1f,1.0f));
    details_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.1f,0.1f,0.1f,1.0f));
    details_material->setShininess(osg::Material::FRONT,15.0f);

    floor_material->setEmission(osg::Material::FRONT,osg::Vec4(1.8f,1.8f,1.8f,1.0f));
    floor_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.9f,0.75f,0.2f,1.0f));
    floor_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.02f,0.02f,0.01f,1.0f));
    floor_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.02f,0.02f,0.01f,1.0f));
    floor_material->setShininess(osg::Material::FRONT,5.0f);

    glass_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    glass_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.0f,0.0f,0.0f,1.0f));
    glass_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.0f,0.0f,0.0f,1.0f));
    glass_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    glass_material->setShininess(osg::Material::FRONT,15.0f);

    lights_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    lights_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.0f,0.0f,0.0f,1.0f));
    lights_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.0f,0.0f,0.0f,1.0f));
    lights_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    lights_material->setShininess(osg::Material::FRONT,15.0f);

    lt_green_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    lt_green_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.65f,0.5f,1.0f));
    lt_green_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.8f,0.5f,1.0f));
    lt_green_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    lt_green_material->setShininess(osg::Material::FRONT,10.0f);

    lt_grey_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    lt_grey_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.4f,0.4f,0.4f,1.0f));
    lt_grey_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    lt_grey_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.2f,0.2f,0.2f,1.0f));
    lt_grey_material->setShininess(osg::Material::FRONT,5.0f);

    orange_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    orange_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.5f,0.1f,0.1f,1.0f));
    orange_material->setDiffuse(osg::Material::FRONT,osg::Vec4(1.0f,0.65f,0.3f,1.0f));
    orange_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    orange_material->setShininess(osg::Material::FRONT,10.0f);

    red_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    red_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.0f,0.0f,0.0f,1.0f));
    red_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.0f,0.0f,0.0f,1.0f));
    red_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    red_material->setShininess(osg::Material::FRONT,15.0f);

    red_brown_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    red_brown_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.3f,0.25f,0.2f,1.0f));
    red_brown_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.14f,0.07f,0.0f,1.0f));
    red_brown_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.01f,0.01f,0.01f,1.0f));
    red_brown_material->setShininess(osg::Material::FRONT,10.0f);

    walls_material->setEmission(osg::Material::FRONT,osg::Vec4(3.4f,3.4f,3.4f,1.0f));
    walls_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.85f,0.7f,0.3f,1.0f));
    walls_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.02f,0.02f,0.01f,1.0f));
    walls_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.01f,0.01f,0.01f,1.0f));
    walls_material->setShininess(osg::Material::FRONT,5.0f);

    white_pipes_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    white_pipes_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.4f,0.4f,0.4f,1.0f));
    white_pipes_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.9f,0.9f,0.95f,1.0f));
    white_pipes_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    white_pipes_material->setShininess(osg::Material::FRONT,15.0f);

    white_ducts_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    white_ducts_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.4f,0.4f,0.4f,1.0f));
    white_ducts_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.9f,0.9f,0.95f,1.0f));
    white_ducts_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    white_ducts_material->setShininess(osg::Material::FRONT,10.0f);

    yellow_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    yellow_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.3f,1.0f));
    yellow_material->setDiffuse(osg::Material::FRONT,osg::Vec4(1.0f,0.85f,0.3f,1.0f));
    yellow_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    yellow_material->setShininess(osg::Material::FRONT,10.0f);

    frame_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    frame_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.1f,0.1f,0.1f,1.0f));
    frame_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.1f,0.1f,0.1f,1.0f));
    frame_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    frame_material->setShininess(osg::Material::FRONT,12.0f);

    railing_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    railing_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.3f,1.0f));
    railing_material->setDiffuse(osg::Material::FRONT,osg::Vec4(1.0f,0.85f,0.3f,1.0f));
    railing_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    railing_material->setShininess(osg::Material::FRONT,10.0f);

    plenum_piping_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    plenum_piping_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    plenum_piping_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    plenum_piping_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    plenum_piping_material->setShininess(osg::Material::FRONT,10.0f);

    blower_components_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    blower_components_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    blower_components_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    blower_components_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    blower_components_material->setShininess(osg::Material::FRONT,10.0f);

    brackets_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    brackets_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    brackets_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    brackets_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    brackets_material->setShininess(osg::Material::FRONT,10.0f);

    cement_base_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    cement_base_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    cement_base_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    cement_base_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    cement_base_material->setShininess(osg::Material::FRONT,10.0f);

    combustor_piping_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    combustor_piping_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    combustor_piping_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    combustor_piping_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    combustor_piping_material->setShininess(osg::Material::FRONT,10.0f);

    compressor_inlet_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    compressor_inlet_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    compressor_inlet_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    compressor_inlet_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    compressor_inlet_material->setShininess(osg::Material::FRONT,10.0f);

    heat_exchanger_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    heat_exchanger_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    heat_exchanger_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    heat_exchanger_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    heat_exchanger_material->setShininess(osg::Material::FRONT,10.0f);

    heat_exchanger_sweep_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    heat_exchanger_sweep_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    heat_exchanger_sweep_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    heat_exchanger_sweep_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    heat_exchanger_sweep_material->setShininess(osg::Material::FRONT,10.0f);

    load_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    load_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    load_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    load_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    load_material->setShininess(osg::Material::FRONT,10.0f);

    plenum_system_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    plenum_system_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    plenum_system_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    plenum_system_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    plenum_system_material->setShininess(osg::Material::FRONT,10.0f);

    relief_piping_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    relief_piping_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    relief_piping_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    relief_piping_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    relief_piping_material->setShininess(osg::Material::FRONT,10.0f);

    shell_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    shell_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    shell_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    shell_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    shell_material->setShininess(osg::Material::FRONT,10.0f);

    stack_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    stack_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    stack_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    stack_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    stack_material->setShininess(osg::Material::FRONT,10.0f);

    turbine_exhaust_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    turbine_exhaust_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    turbine_exhaust_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    turbine_exhaust_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    turbine_exhaust_material->setShininess(osg::Material::FRONT,10.0f);

    turbine_postcombustor_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    turbine_postcombustor_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    turbine_postcombustor_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    turbine_postcombustor_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    turbine_postcombustor_material->setShininess(osg::Material::FRONT,10.0f);

    miscellaneous_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    miscellaneous_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    miscellaneous_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    miscellaneous_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    miscellaneous_material->setShininess(osg::Material::FRONT,10.0f);
}
////////////////////////////////////////////////////////////////////////////////
void Scene::CreateShadowTexture()
{
    unsigned int tex_width = 4096;
    unsigned int tex_height = 4096;

    //Create the shadow texture
    shadow->setTextureSize( tex_width, tex_height );
    shadow->setInternalFormat( GL_DEPTH_COMPONENT );
    shadow->setSourceType( GL_UNSIGNED_INT );

    shadow->setShadowComparison( true );
    shadow->setShadowCompareFunc( osg::Texture::LEQUAL );

    shadow->setShadowTextureMode( osg::Texture::LUMINANCE );
    shadow->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
    shadow->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
    shadow->setWrap( osg::Texture::WRAP_S, osg::Texture::CLAMP_TO_EDGE );
    shadow->setWrap( osg::Texture::WRAP_T, osg::Texture::CLAMP_TO_EDGE );

    //Set up the "render to texture" camera
    {
        //Create the camera
        camera->setClearMask( GL_DEPTH_BUFFER_BIT );
        camera->setClearColor( osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        camera->setComputeNearFarMode(osg::Camera::DO_NOT_COMPUTE_NEAR_FAR);

        //Set viewport
        camera->setViewport(0,0,tex_width,tex_height);

        osg::ref_ptr<osg::StateSet> local_stateset=camera->getOrCreateStateSet();
        local_stateset->setMode(GL_LIGHTING,osg::StateAttribute::OFF);

        float factor=0.0f;
        float units=1.0f;

        osg::ref_ptr<osg::PolygonOffset> polygon_offset = new osg::PolygonOffset();
        polygon_offset->setFactor(factor);
        polygon_offset->setUnits(units);
        local_stateset->setAttribute(polygon_offset.get(),osg::StateAttribute::ON|osg::StateAttribute::OVERRIDE);
        local_stateset->setMode(GL_POLYGON_OFFSET_FILL,osg::StateAttribute::ON|osg::StateAttribute::OVERRIDE);

        osg::ref_ptr<osg::CullFace> cull_face=new osg::CullFace;
        cull_face->setMode(osg::CullFace::FRONT);
        local_stateset->setAttribute(cull_face.get(),osg::StateAttribute::ON|osg::StateAttribute::OVERRIDE);
        local_stateset->setMode(GL_CULL_FACE,osg::StateAttribute::ON|osg::StateAttribute::OVERRIDE);

        //Set the camera to render before the main camera
        camera->setRenderOrder(osg::Camera::PRE_RENDER);

        //Tell the camera to use OpenGL frame buffer object where supported
        camera->setRenderTargetImplementation(osg::Camera::FRAME_BUFFER_OBJECT);

        //Attach the texture and use it as the color buffer
        camera->attach(osg::Camera::DEPTH_BUFFER,shadow.get());

        //Add subgraph to render
        camera->addChild(shadowed_scene.get());

        //Create the texgen node to project the tex coords onto the subgraph  
        texgenNode->setTextureUnit(0);

        osg::BoundingSphere bs;
        for(unsigned int i=0;i<camera->getNumChildren();++i)
        {
            bs.expandBy(camera->getChild(i)->getBound());
        }

        osg::Vec3 position=light_transform_1->getMatrix().getTrans();

        float centerDistance=(position-bs.center()).length();

        float znear=centerDistance-bs.radius();
        float zfar=centerDistance+bs.radius();
        float zNearRatio=0.001f;
        if(znear<zfar*zNearRatio)
        {
            znear=zfar*zNearRatio;
        }

        float top = ( bs.radius() / centerDistance ) * znear;
        float right=top;

        camera->setReferenceFrame(osg::Camera::ABSOLUTE_RF);
        camera->setProjectionMatrixAsFrustum(-right,right,-top,top,znear,zfar);
        camera->setViewMatrixAsLookAt(position,bs.center(),osg::Vec3(0.0f,1.0f,0.0f));

        //Compute the matrix which takes a vertex from local coords into tex coords
        osg::Matrix MVPT=camera->getViewMatrix()*
        camera->getProjectionMatrix()*
        osg::Matrix::translate(1.0f,1.0f,1.0f)*
        osg::Matrix::scale(0.5f,0.5f,0.5f);

        //Texture Generation
        texgenNode->getTexGen()->setMode(osg::TexGen::EYE_LINEAR);
        texgenNode->getTexGen()->setPlanesFromMatrix(MVPT);
    }

    //return shadow.get();
}
////////////////////////////////////////////////////////////////////////////////
// Implementation from Chapter 17, Efficient Soft-Edged Shadows Using Pixel Shader Branching, Yury Uralsky.
// GPU Gems 2, Matt Pharr ed. Addison-Wesley.
//
// Creates a 3D texture containing jittering data used in the shader to take samples of the shadow map.
void Scene::CreateJitterTexture()
{
    //Create a 3D texture with hw mipmapping
    jitter->setFilter( osg::Texture3D::MIN_FILTER, osg::Texture3D::NEAREST );
    jitter->setFilter( osg::Texture3D::MAG_FILTER, osg::Texture3D::NEAREST );
    jitter->setWrap( osg::Texture3D::WRAP_S, osg::Texture3D::REPEAT );
    jitter->setWrap( osg::Texture3D::WRAP_T, osg::Texture3D::REPEAT );
    jitter->setWrap( osg::Texture3D::WRAP_R, osg::Texture3D::REPEAT );
    jitter->setUseHardwareMipMapGeneration( true );

    const unsigned int size = 16;
    const unsigned int gridW = 8;
    const unsigned int gridH = 8;
    unsigned int R = ( gridW * gridH / 2 );
    jitter->setTextureSize( size, size, R );

    //Then create the 3d image to fill with jittering data
    osg::ref_ptr< osg::Image > image3D = new osg::Image();
    unsigned char* data3D = new unsigned char[ size * size * R * 4 ];

    for( unsigned int s = 0; s < size; ++s )
    {
        for( unsigned int t = 0; t < size; ++t )
        {
            float v[ 4 ], d[ 4 ];

            for( unsigned int r = 0; r < R; ++r )
            {
                const int x = r % ( gridW / 2 );
                const int y = ( gridH - 1 ) - ( r / (gridW / 2) );

                //Generate points on a  regular gridW x gridH rectangular
                //grid.   We  multiply  x   by  2  because,  we  treat  2
                //consecutive x  each loop iteration.  Add 0.5f  to be in
                //the center of the pixel. x, y belongs to [ 0.0, 1.0 ].
                v[0] = static_cast< float >( x * 2     + 0.5f ) / gridW;
                v[1] = static_cast< float >( y         + 0.5f ) / gridH;
                v[2] = static_cast< float >( x * 2 + 1 + 0.5f ) / gridW;
                v[3] = v[ 1 ];

                //Jitter positions. ( 0.5f / w ) == ( 1.0f / 2*w )
                v[0] += ( static_cast< float >( rand() ) * 2.f / RAND_MAX - 1.f ) * ( 0.5f / gridW );
                v[1] += ( static_cast< float >( rand() ) * 2.f / RAND_MAX - 1.f ) * ( 0.5f / gridH );
                v[2] += ( static_cast< float >( rand() ) * 2.f / RAND_MAX - 1.f ) * ( 0.5f / gridW );
                v[3] += ( static_cast< float >( rand() ) * 2.f / RAND_MAX - 1.f ) * ( 0.5f / gridH );

                //Warp to disk; values in [-1,1]
                d[0] = sqrtf( v[1] ) * cosf( 2.f * 3.1415926f * v[ 0 ] );
                d[1] = sqrtf( v[1] ) * sinf( 2.f * 3.1415926f * v[ 0 ] );
                d[2] = sqrtf( v[3] ) * cosf( 2.f * 3.1415926f * v[ 2 ] );
                d[3] = sqrtf( v[3] ) * sinf( 2.f * 3.1415926f * v[ 2 ] );

                //store d into unsigned values [0,255]
                const unsigned int tmp = ( ( r * size * size ) + ( t * size ) + s ) * 4;
                data3D[ tmp + 0 ] = static_cast< unsigned char >( ( 1.f + d[ 0 ] ) * 127  );
                data3D[ tmp + 1 ] = static_cast< unsigned char >( ( 1.f + d[ 1 ] ) * 127  );
                data3D[ tmp + 2 ] = static_cast< unsigned char >( ( 1.f + d[ 2 ] ) * 127  );
                data3D[ tmp + 3 ] = static_cast< unsigned char >( ( 1.f + d[ 3 ] ) * 127  );
            }
        }
    }

    //The GPU Gem implementation uses a NV specific internal texture format (GL_SIGNED_RGBA_NV)
    //In order to make it more generic, we use GL_RGBA4 which should be cross platform.
    image3D->setImage( size, size, R, GL_RGBA4, GL_RGBA, GL_UNSIGNED_BYTE, data3D, osg::Image::USE_NEW_DELETE );
    jitter->setImage( image3D.get() );

    /*
    ss->setTextureAttributeAndModes((int)_textureUnit + 1, jitter, osg::StateAttribute::ON | osg::StateAttribute::OVERRIDE);
    ss->setTextureMode((int)_textureUnit + 1,GL_TEXTURE_GEN_S,osg::StateAttribute::ON);
    ss->setTextureMode((int)_textureUnit + 1,GL_TEXTURE_GEN_T,osg::StateAttribute::ON);
    ss->setTextureMode((int)_textureUnit + 1,GL_TEXTURE_GEN_R,osg::StateAttribute::ON);
    */
}
////////////////////////////////////////////////////////////////////////////////
void Scene::WriteOutShadow()
{
    osg::ref_ptr<osg::Image> image=new osg::Image;
    image->setInternalTextureFormat(GL_DEPTH_COMPONENT);

    class RGB
    {
    public:
        unsigned char r, g, b;
    };

    /*RGB* pixels;
    pixels=new RGB[3*shadow->getTextureWidth()*shadow->getTextureHeight()*shadow->getTextureDepth()];
    glGetTexImage(GL_TEXTURE_2D,0,GL_LUMINANCE,GL_UNSIGNED_BYTE,pixels);

    glPixelStorei(GL_UNPACK_ALIGNMENT,1);
    glDrawPixels(shadow->getTextureHeight(),shadow->getTextureWidth(),GL_RGB,GL_UNSIGNED_BYTE,pixels);
    image->readPixels(0,0,512,512,GL_LUMINANCE,GL_UNSIGNED_BYTE);
    osgDB::writeImageFile(*image.get(),"./Textures/shadow.bmp");*/
}
////////////////////////////////////////////////////////////////////////////////
void Scene::Base()
{
    for(int i=0;i<(int)shadowed_scene->getNumChildren();i++)
    {
        shader->Base(shadowed_scene->getChild(i));
    }

    for(int i=0;i<(int)non_shadowed_scene->getNumChildren();i++)
    {
        shader->Base(non_shadowed_scene->getChild(i));
    }
}
////////////////////////////////////////////////////////////////////////////////
void Scene::Phong()
{
    for(int i=0;i<(int)shadowed_scene->getNumChildren();i++){
        shader->Phong(shadowed_scene->getChild(i));
    }

    for(int i=0;i<(int)non_shadowed_scene->getNumChildren();i++){
        shader->Phong(non_shadowed_scene->getChild(i));
    }
}
////////////////////////////////////////////////////////////////////////////////
void Scene::Texture()
{
    shader->Texture(1,ceiling.get());
    shader->Texture(0,details.get());
    shader->Texture(1,floor.get());
    shader->Texture(1,walls.get());

    shader->Base(aluminum_parts.get());
    shader->Base(aluminum_pipes.get());
    shader->Base(black.get());
    shader->Base(brown.get());
    shader->Base(glass.get());
    shader->Base(lights.get());
    shader->Base(lt_green.get());
    shader->Base(lt_grey.get());
    shader->Base(orange.get());
    shader->Base(red.get());
    shader->Base(red_brown.get());
    shader->Base(white_ducts.get());
    shader->Base(white_pipes.get());
    shader->Base(yellow.get());

    shader->Base(frame.get());
    shader->Base(railing.get());
    shader->Base(plenum_piping.get());
    shader->Base(blower_components.get());
    shader->Base(brackets.get());
    shader->Base(cement_base.get());
    shader->Base(combustor_piping.get());
    shader->Base(compressor_inlet.get());
    shader->Base(heat_exchanger.get());
    shader->Base(heat_exchanger_sweep.get());
    shader->Base(load.get());
    shader->Base(plenum_system.get());
    shader->Base(relief_piping.get());
    shader->Base(shell.get());
    shader->Base(stack.get());
    shader->Base(turbine_exhaust.get());
    shader->Base(turbine_postcombustor.get());
    shader->Base(miscellaneous.get());
}
////////////////////////////////////////////////////////////////////////////////
void Scene::PCF()
{
    for(int i=0;i<(int)shadowed_scene->getNumChildren();i++){
      shader->PCF(shadow.get(),shadowed_scene->getChild(i));
    }

    for(int i=0;i<(int)non_shadowed_scene->getNumChildren();i++){
      shader->PCF(shadow.get(),non_shadowed_scene->getChild(i));
    }
}
////////////////////////////////////////////////////////////////////////////////
void Scene::Reflection()
{
    shader->Reflection(0.05f,aluminum_parts.get());
    shader->Reflection(0.05f,aluminum_pipes.get());
    shader->Reflection(0.05f,black.get());
    shader->Reflection(0.05f,brown.get());
    shader->Reflection(0.05f,ceiling.get());
    shader->Reflection(0.05f,details.get());
    shader->Reflection(0.05f,floor.get());
    shader->Reflection(0.05f,glass.get());
    shader->Reflection(0.05f,lights.get());
    shader->Reflection(0.05f,lt_green.get());
    shader->Reflection(0.05f,lt_grey.get());
    shader->Reflection(0.05f,orange.get());
    shader->Reflection(0.05f,red.get());
    shader->Reflection(0.05f,red_brown.get());
    shader->Reflection(0.05f,walls.get());
    shader->Reflection(0.05f,white_ducts.get());
    shader->Reflection(0.05f,white_pipes.get());
    shader->Reflection(0.05f,yellow.get());

    shader->Reflection(0.05f,frame.get());
    shader->Reflection(0.05f,railing.get());
    shader->Reflection(0.05f,plenum_piping.get());
    shader->Reflection(0.05f,blower_components.get());
    shader->Reflection(0.05f,brackets.get());
    shader->Reflection(0.05f,cement_base.get());
    shader->Reflection(0.05f,combustor_piping.get());
    shader->Reflection(0.05f,compressor_inlet.get());
    shader->Reflection(0.05f,heat_exchanger.get());
    shader->Reflection(0.05f,heat_exchanger_sweep.get());
    shader->Reflection(0.05f,load.get());
    shader->Reflection(0.05f,plenum_system.get());
    shader->Reflection(0.05f,relief_piping.get());
    shader->Reflection(0.05f,shell.get());
    shader->Reflection(0.05f,stack.get());
    shader->Reflection(0.05f,turbine_exhaust.get());
    shader->Reflection(0.05f,turbine_postcombustor.get());
    shader->Reflection(0.05f,miscellaneous.get());
}
////////////////////////////////////////////////////////////////////////////////
void Scene::XRay()
{
    for(int i=0;i<(int)shadowed_scene->getNumChildren();i++){
      shader->XRay(shadowed_scene->getChild(i));
    }

    for(int i=0;i<(int)non_shadowed_scene->getNumChildren();i++){
      shader->XRay(non_shadowed_scene->getChild(i));
    }
}
////////////////////////////////////////////////////////////////////////////////
void Scene::PhongTexture()
{
    shader->Phong(aluminum_parts.get());
    shader->Phong(aluminum_pipes.get());
    shader->Phong(black.get());
    shader->Phong(brown.get());
    shader->Phong_Texture(1,ceiling.get());
    shader->Phong_Texture(0,details.get());
    shader->Phong_Texture(1,floor.get());
    shader->Phong(glass.get());
    shader->Phong(lights.get());
    shader->Phong(lt_green.get());
    shader->Phong(lt_grey.get());
    shader->Phong(orange.get());
    shader->Phong(red.get());
    shader->Phong(red_brown.get());
    shader->Phong_Texture(1,walls.get());
    shader->Phong(white_ducts.get());
    shader->Phong(white_pipes.get());
    shader->Phong(yellow.get());

    shader->Phong(frame.get());
    shader->Phong(railing.get());
    shader->Phong(plenum_piping.get());
}
////////////////////////////////////////////////////////////////////////////////
void Scene::PhongPCF()
{
    shader->Phong_PCF(shadow.get(),aluminum_parts.get());
    shader->Phong_PCF(shadow.get(),aluminum_pipes.get());
    shader->Phong_PCF(shadow.get(),black.get());
    shader->Phong_PCF(shadow.get(),brown.get());
    shader->Phong_PCF(shadow.get(),ceiling.get());
    shader->Phong_PCF(shadow.get(),details.get());
    shader->Phong_PCF(shadow.get(),floor.get());
    shader->Phong_PCF(shadow.get(),glass.get());
    shader->Phong_PCF(shadow.get(),lights.get());
    shader->Phong_PCF(shadow.get(),lt_green.get());
    shader->Phong_PCF(shadow.get(),lt_grey.get());
    shader->Phong_PCF(shadow.get(),orange.get());
    shader->Phong_PCF(shadow.get(),red.get());
    shader->Phong_PCF(shadow.get(),red_brown.get());
    shader->Phong_PCF(shadow.get(),walls.get());
    shader->Phong_PCF(shadow.get(),white_ducts.get());
    shader->Phong_PCF(shadow.get(),white_pipes.get());
    shader->Phong_PCF(shadow.get(),yellow.get());

    shader->Phong_PCF(shadow.get(),frame.get());
    shader->Phong_PCF(shadow.get(),railing.get());
    shader->Phong_PCF(shadow.get(),plenum_piping.get());
}
////////////////////////////////////////////////////////////////////////////////
void Scene::PhongReflection()
{
    shader->Phong(aluminum_parts.get());
    shader->Phong(aluminum_pipes.get());
    shader->Phong(black.get());
    shader->Phong(brown.get());
    shader->Phong(ceiling.get());
    shader->Phong(details.get());
    shader->Phong(floor.get());
    shader->Phong(glass.get());
    shader->Phong(lights.get());
    shader->Phong(lt_green.get());
    shader->Phong(lt_grey.get());
    shader->Phong(orange.get());
    shader->Phong(red.get());
    shader->Phong(red_brown.get());
    shader->Phong(walls.get());
    shader->Phong(white_ducts.get());
    shader->Phong(white_pipes.get());
    shader->Phong(yellow.get());

    shader->Phong_PCF(shadow.get(),frame.get());
    shader->Phong_PCF(shadow.get(),railing.get());
    shader->Phong_PCF(shadow.get(),plenum_piping.get());
}
////////////////////////////////////////////////////////////////////////////////
void Scene::TexturePCF()
{
    shader->Texture_PCF(1,shadow.get(),ceiling.get());
    shader->Texture_PCF(0,shadow.get(),details.get());
    shader->Texture_PCF(1,shadow.get(),floor.get());
    shader->Texture_PCF(1,shadow.get(),walls.get());

    shader->PCF(shadow.get(),aluminum_parts.get());
    shader->PCF(shadow.get(),aluminum_pipes.get());
    shader->PCF(shadow.get(),black.get());
    shader->PCF(shadow.get(),brown.get());
    shader->PCF(shadow.get(),glass.get());
    shader->PCF(shadow.get(),lights.get());
    shader->PCF(shadow.get(),lt_green.get());
    shader->PCF(shadow.get(),lt_grey.get());
    shader->PCF(shadow.get(),orange.get());
    shader->PCF(shadow.get(),red.get());
    shader->PCF(shadow.get(),red_brown.get());
    shader->PCF(shadow.get(),white_ducts.get());
    shader->PCF(shadow.get(),white_pipes.get());
    shader->PCF(shadow.get(),yellow.get());

    shader->Phong_PCF(shadow.get(),frame.get());
    shader->Phong_PCF(shadow.get(),railing.get());
    shader->Phong_PCF(shadow.get(),plenum_piping.get());
}
////////////////////////////////////////////////////////////////////////////////
void Scene::TextureReflection()
{
    shader->Texture(1,ceiling.get());
    shader->Texture_Reflection(0,1.0f,details.get());
    shader->Texture_Reflection(1,1.0f,floor.get());
    shader->Texture(1,walls.get());

    shader->Base(aluminum_parts.get());
    shader->Base(aluminum_pipes.get());
    shader->Base(black.get());
    shader->Base(brown.get());
    shader->Base(glass.get());
    shader->Base(lights.get());
    shader->Base(lt_green.get());
    shader->Base(lt_grey.get());
    shader->Base(orange.get());
    shader->Base(red.get());
    shader->Base(red_brown.get());
    shader->Base(white_ducts.get());
    shader->Base(white_pipes.get());
    shader->Base(yellow.get());

    shader->Base(frame.get());
    shader->Base(railing.get());
    shader->Base(plenum_piping.get());
}
////////////////////////////////////////////////////////////////////////////////
void Scene::PCFReflection()
{
    shader->PCF_Reflection(1.0f,shadow.get(),details.get());
    shader->PCF_Reflection(1.0f,shadow.get(),floor.get());

    shader->Base(aluminum_parts.get());
    shader->Base(aluminum_pipes.get());
    shader->Base(black.get());
    shader->Base(brown.get());
    shader->PCF(shadow.get(),ceiling.get());
    shader->Base(glass.get());
    shader->Base(lights.get());
    shader->Base(lt_green.get());
    shader->Base(lt_grey.get());
    shader->Base(orange.get());
    shader->Base(red.get());
    shader->Base(red_brown.get());
    shader->PCF(shadow.get(),walls.get());
    shader->Base(white_ducts.get());
    shader->Base(white_pipes.get());
    shader->Base(yellow.get());

    shader->Base(frame.get());
    shader->Base(railing.get());
    shader->Base(plenum_piping.get());
}
////////////////////////////////////////////////////////////////////////////////
void Scene::PhongTexturePCF()
{

}
////////////////////////////////////////////////////////////////////////////////
void Scene::PhongTextureReflection()
{

}
////////////////////////////////////////////////////////////////////////////////
void Scene::PhongPCFReflection()
{

}
////////////////////////////////////////////////////////////////////////////////
void Scene::TexturePCFReflection()
{

}
////////////////////////////////////////////////////////////////////////////////
void Scene::PhongTexturePCFReflection()
{
    shader->Phong_Texture_PCF(1,shadow.get(),ceiling.get());
    shader->Phong_Texture_PCF(1,shadow.get(),walls.get());

    shader->Phong_Texture_PCF_Reflection(0,0.05f,shadow.get(),details.get());
    shader->Phong_Texture_PCF_Reflection(1,0.05f,shadow.get(),floor.get());


    shader->Phong_PCF_Reflection(0.05f,shadow.get(),aluminum_parts.get());
    shader->Phong_PCF_Reflection(0.05f,shadow.get(),aluminum_pipes.get());
    shader->Phong_PCF_Reflection(0.05f,shadow.get(),black.get());
    shader->Phong_PCF_Reflection(0.05f,shadow.get(),brown.get());
    shader->Phong_PCF_Reflection(0.05f,shadow.get(),glass.get());
    shader->Phong_PCF_Reflection(0.05f,shadow.get(),lights.get());
    shader->Phong_PCF_Reflection(0.05f,shadow.get(),lt_green.get());
    shader->Phong_PCF_Reflection(0.05f,shadow.get(),lt_grey.get());
    shader->Phong_PCF_Reflection(0.05f,shadow.get(),orange.get());
    shader->Phong_PCF_Reflection(0.05f,shadow.get(),red.get());
    shader->Phong_PCF_Reflection(0.05f,shadow.get(),red_brown.get());
    shader->Phong_PCF_Reflection(0.05f,shadow.get(),white_ducts.get());
    shader->Phong_PCF_Reflection(0.05f,shadow.get(),white_pipes.get());
    shader->Phong_PCF_Reflection(0.05f,shadow.get(),yellow.get());

    shader->Phong_PCF_Reflection(0.05f,shadow.get(),frame.get());
    shader->Phong_PCF_Reflection(0.05f,shadow.get(),railing.get());
    shader->Phong_PCF_Reflection(0.05f,shadow.get(),plenum_piping.get());
    shader->Phong_PCF_Reflection(0.05f,shadow.get(),blower_components.get());
    shader->Phong_PCF_Reflection(0.05f,shadow.get(),brackets.get());
    shader->Phong_PCF_Reflection(0.05f,shadow.get(),cement_base.get());
    shader->Phong_PCF_Reflection(0.05f,shadow.get(),combustor_piping.get());
    shader->Phong_PCF_Reflection(0.05f,shadow.get(),compressor_inlet.get());
    shader->Phong_PCF_Reflection(0.05f,shadow.get(),heat_exchanger.get());
    shader->Phong_PCF_Reflection(0.05f,shadow.get(),heat_exchanger_sweep.get());
    shader->Phong_PCF_Reflection(0.05f,shadow.get(),load.get());
    shader->Phong_PCF_Reflection(0.05f,shadow.get(),plenum_system.get());
    shader->Phong_PCF_Reflection(0.05f,shadow.get(),relief_piping.get());
    shader->Phong_PCF_Reflection(0.05f,shadow.get(),shell.get());
    shader->Phong_PCF_Reflection(0.05f,shadow.get(),stack.get());
    shader->Phong_PCF_Reflection(0.05f,shadow.get(),turbine_exhaust.get());
    shader->Phong_PCF_Reflection(0.05f,shadow.get(),turbine_postcombustor.get());
    shader->Phong_PCF_Reflection(0.05f,shadow.get(),miscellaneous.get());
}
////////////////////////////////////////////////////////////////////////////////
