/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/

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
PluginBase(),
m_scene( 0 )
{
    mObjectName = "HyperLabUI";

    mEventHandlerMap[ "SHADER_EFFECTS_UPDATE" ] = this;
    mEventHandlerMap[ "AMBIENT_UPDATE" ] = this;
    mEventHandlerMap[ "DIFFUSE_UPDATE" ] = this;
    mEventHandlerMap[ "SPECULAR_UPDATE" ] = this;
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
    PluginBase::InitializeNode( veworldDCS );

    m_scene = new hyperlab::Scene( mDCS.get(), mPhysicsSimulator );
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabGP::PreFrameUpdate()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabGP::UpdateParams()
{
    mXmlModel->GetInput( "portNumber" )->GetDataValuePair( "portNumber" )->GetData( _portNumber );

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
