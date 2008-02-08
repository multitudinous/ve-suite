// --- My Includes --- //
#include "HyperLabGP.h"
#include "Scene.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

// --- OSG Includes --- //
#include <osg/Light>

// --- C/C++ Libraries --- //
#include <fstream>
#include <map>
#include <cstdlib>

////////////////////////////////////////////////////////////////////////////////
HyperLabGP::HyperLabGP()
:
cfdVEBaseClass(),
m_scene( 0 )
{
    m_objectName = "HyperLabUI";

    m_ehMap[ "SHADER_EFFECTS_UPDATE" ] = this;
    m_ehMap[ "AMBIENT_UPDATE" ] = this;
    m_ehMap[ "DIFFUSE_UPDATE" ] = this;
    m_ehMap[ "SPECULAR_UPDATE" ] = this;
}
////////////////////////////////////////////////////////////////////////////////
HyperLabGP::~HyperLabGP()
{
    if( m_scene )
    {
        delete m_scene;
    }
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabGP::InitializeNode( ves::xplorer::scenegraph::DCS* veworldDCS )
{
    cfdVEBaseClass::InitializeNode( veworldDCS );

    m_scene = new hyperlab::Scene( m_dcs.get(), m_physicsSimulator );
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabGP::PreFrameUpdate()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabGP::UpdateParams()
{
    m_xmlModel->GetInput( "portNumber" )->GetDataValuePair( "portNumber" )->GetData( _portNumber );

    //_excelData = socket.GetSensorData();
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabGP::SetCurrentCommand( ves::open::xml::Command* command )
{
    if( !command )
    {
        return;
    }

    double data[ 3 ];

    /*
    //Set current shader effect
    if( command->GetCommandName() == "SHADER_EFFECTS_UPDATE" )
    {

    }
    else if( command->GetCommandName() == "AMBIENT_UPDATE" )
    {
        command->GetDataValuePair( "ar_color" )->GetData( _ar_color );
        command->GetDataValuePair( "ag_color" )->GetData( _ag_color );
        command->GetDataValuePair( "ab_color" )->GetData( _ab_color );

        root->light_1->setAmbient( osg::Vec4( _ar_color, _ag_color, _ab_color, 1.0f ) );
    }
    else if( command->GetCommandName() == "DIFFUSE_UPDATE" )
    {
        command->GetDataValuePair( "dr_color" )->GetData( _dr_color );
        command->GetDataValuePair( "dg_color" )->GetData( _dg_color );
        command->GetDataValuePair( "db_color" )->GetData( _db_color );

        root->light_1->setDiffuse( osg::Vec4( _dr_color, _dg_color, _db_color, 1.0f ) );
    }
    else if( command->GetCommandName() == "SPECULAR_UPDATE" )
    {
        command->GetDataValuePair( "sr_color" )->GetData( _sr_color );
        command->GetDataValuePair( "sg_color" )->GetData( _sg_color );
        command->GetDataValuePair( "sb_color" )->GetData( _sb_color );

        root->light_1->setSpecular( osg::Vec4( _sr_color, _sg_color, _sb_color, 1.0f ) );
    }
    */
}
////////////////////////////////////////////////////////////////////////////////
