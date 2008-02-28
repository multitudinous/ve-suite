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
void HyperLabGP::SetCurrentCommand( ves::open::xml::CommandPtr command )
{
    if( !command )
    {
        return;
    }

    double data[ 3 ] = { 0 };

    //Set current shader effect
    if( command->GetCommandName() == "SHADER_EFFECTS_UPDATE" )
    {
        unsigned int temp = 0;
        command->GetDataValuePair( "shaderEffects" )->GetData( temp );

        if( temp == 0 )
        {
            m_scene->DefaultVisuals();
        }
        else if( temp == 1 )
        {
            m_scene->AdvancedVisuals();
        }
        else if( temp == 2 )
        {
            m_scene->XRay();
        }
    }
    else if( command->GetCommandName() == "AMBIENT_UPDATE" )
    {
        command->GetDataValuePair( "arColor" )->GetData( data[ 0 ] );
        command->GetDataValuePair( "agColor" )->GetData( data[ 1 ] );
        command->GetDataValuePair( "abColor" )->GetData( data[ 2 ] );

        m_scene->GetLight()->setAmbient( osg::Vec4( data[ 0 ], data[ 1 ], data[ 2 ], 1.0f ) );
    }
    else if( command->GetCommandName() == "DIFFUSE_UPDATE" )
    {
        command->GetDataValuePair( "drColor" )->GetData( data[ 0 ] );
        command->GetDataValuePair( "dgColor" )->GetData( data[ 1 ] );
        command->GetDataValuePair( "dbColor" )->GetData( data[ 2 ] );

        m_scene->GetLight()->setDiffuse( osg::Vec4( data[ 0 ], data[ 1 ], data[ 2 ], 1.0f ) );
    }
    else if( command->GetCommandName() == "SPECULAR_UPDATE" )
    {
        command->GetDataValuePair( "srColor" )->GetData( data[ 0 ] );
        command->GetDataValuePair( "sgColor" )->GetData( data[ 1 ] );
        command->GetDataValuePair( "sbColor" )->GetData( data[ 2 ] );

        m_scene->GetLight()->setSpecular( osg::Vec4( data[ 0 ], data[ 1 ], data[ 2 ], 1.0f ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
