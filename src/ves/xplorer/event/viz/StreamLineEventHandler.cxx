/*************** <auto-copyright.rb BEGIN do not edit this line> *************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> **************/
// --- VE-Suite Includes --- //
#include <ves/xplorer/event/viz/StreamLineEventHandler.h>
#include <ves/xplorer/event/viz/cfdGraphicsObject.h>

#include <ves/xplorer/SteadyStateVizHandler.h>

#include <ves/xplorer/Model.h>
#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/GlobalBase.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

// --- C/C++ Libraries --- //
#include <vector>
#include <iostream>

using namespace ves::xplorer::event;

////////////////////////////////////////////////////////////////////////////////
StreamLineEventHandler::StreamLineEventHandler()
        :
        ves::xplorer::event::EventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
StreamLineEventHandler::StreamLineEventHandler( const StreamLineEventHandler& rhs )
        :
        ves::xplorer::event::EventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
StreamLineEventHandler::~StreamLineEventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void StreamLineEventHandler::SetGlobalBaseObject( ves::xplorer::GlobalBase* modelHandler )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void StreamLineEventHandler::Execute( const ves::open::xml::XMLObjectPtr& veXMLObject )
{
    std::vector< ves::xplorer::cfdGraphicsObject* > graphicsObject =
        ves::xplorer::SteadyStateVizHandler::instance()->
        GetGraphicsObjectsOfType( STREAMLINES );
    
    if( graphicsObject.empty() )
    {
        return;
    }

    ves::open::xml::CommandPtr command = 
        boost::dynamic_pointer_cast<ves::open::xml::Command>( veXMLObject );
    ves::open::xml::DataValuePairPtr sizeDVP = 
        command->GetDataValuePair( "Size" );
    ves::open::xml::DataValuePairPtr glowDVP = 
        command->GetDataValuePair( "Fade Time" );
    ves::open::xml::DataValuePairPtr animationSpeedDVP = 
        command->GetDataValuePair( "Animation Speed" );
    
    if( sizeDVP )
    {
        double size;
        sizeDVP->GetData( size );
    	float range = 2.5f;
        int diameter = static_cast< int >( size );
    	float localLineDiameter = exp( diameter / ( 100.0 / range ) ) * 1.0f * 0.001f;

    	// this is to normalize -100 to 100 on the GUI  to  1-21 for diameters
    	// note that multiplying by 0.005 is the same as dividing by 200, or the range
    	size = ( diameter + 110 ) * 0.005 *  20;

        UpdateGeodeUniform( graphicsObject, sizeDVP, "particleSize", size );
    }

    if( glowDVP )
    {
        double uniformVal = 0.0;
        glowDVP->GetData( uniformVal );
        uniformVal *= 0.1;
        UpdateGeodeUniform( graphicsObject, glowDVP, "fadeTime", uniformVal );
    }

    if( animationSpeedDVP )
    {
        double uniformVal = 0.0;
        animationSpeedDVP->GetData( uniformVal );
        uniformVal *= 0.1;
        UpdateGeodeUniform( graphicsObject, animationSpeedDVP, "repeatTime", uniformVal );
    }
}
////////////////////////////////////////////////////////////////////////////////
StreamLineEventHandler& StreamLineEventHandler::operator=( const StreamLineEventHandler& rhs )
{
    if( this != &rhs )
    {
        ves::xplorer::event::EventHandler::operator=( rhs );
    }

    return *this;
}
////////////////////////////////////////////////////////////////////////////////
void StreamLineEventHandler::UpdateGeodeUniform( 
    const std::vector< ves::xplorer::cfdGraphicsObject* >& graphicsObject, 
    ves::open::xml::DataValuePairPtr dvp, 
    const std::string& uniformName, double valueFactor )
{
    unsigned int numdraw = 0.0;
    osg::ref_ptr< osg::Uniform > activeUniform;
    osg::ref_ptr< ves::xplorer::scenegraph::Geode > geode;
    
    for( size_t i = 0; i < graphicsObject.size(); ++i )
    {
        std::vector< osg::ref_ptr< ves::xplorer::scenegraph::Geode > > 
            geodes = graphicsObject.at( i )->GetGeodes();
        for( size_t j = 0; j < geodes.size(); ++j )
        {
            geode = geodes.at( j );
            numdraw = geode->getNumDrawables();
            for( size_t k = 0; k < numdraw; ++k )
            {
                activeUniform = geode->getDrawable( k )->
                    getStateSet()->getUniform( uniformName );
                if( activeUniform.valid() )
                {
                    activeUniform->set( static_cast< float >( valueFactor ) );
                }
            }
        }
    }
}