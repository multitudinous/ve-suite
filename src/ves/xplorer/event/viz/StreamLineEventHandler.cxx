/*************** <auto-copyright.pl BEGIN do not edit this line> *************
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
 *************** <auto-copyright.pl END do not edit this line> **************/
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

// --- Juggler Includes --- //
#include <boost/filesystem/operations.hpp>   //includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>

#ifdef WIN32
#include <direct.h>
#else
#include <unistd.h>
#endif

// --- C/C++ Libraries --- //
#include <vector>

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
void StreamLineEventHandler::Execute( ves::open::xml::XMLObject* veXMLObject )
{
    ves::open::xml::Command* command = dynamic_cast< ves::open::xml::Command* >( veXMLObject );
    ves::open::xml::DataValuePairWeakPtr sizeDVP = command->GetDataValuePair( "Size" );
    ves::open::xml::DataValuePairWeakPtr glowDVP = command->GetDataValuePair( "Glow" );

    double size, glow;
    std::vector< ves::xplorer::cfdGraphicsObject* > cfdGraphicsObject =
        ves::xplorer::SteadyStateVizHandler::instance()->
        GetGraphicsObjectsOfType( STREAMLINES );

    if( sizeDVP && !cfdGraphicsObject.empty() )
    {
        sizeDVP->GetData( size );
        size /= 200.0;
        for( size_t i = 0; i < cfdGraphicsObject.size(); ++i )
        {
            std::vector< osg::ref_ptr< ves::xplorer::scenegraph::Geode > > geodes =
                cfdGraphicsObject.at( i )->GetGeodes();
            for( size_t j = 0; j < geodes.size(); ++j )
            {
                osg::ref_ptr< osg::Uniform > parSize =
                    geodes.at( j )->getDrawable( 0 )->getStateSet()->getUniform( "particleSize" );
                if( parSize.valid() )
                {
                    parSize->set( static_cast< float >( size ) );
                }
            }
        }
    }

    if( glowDVP && !cfdGraphicsObject.empty() )
    {
        glowDVP->GetData( glow );
        glow /= 100.0;
        for( size_t i = 0; i < cfdGraphicsObject.size(); ++i )
        {
            std::vector< osg::ref_ptr< ves::xplorer::scenegraph::Geode > > geodes =
                cfdGraphicsObject.at( i )->GetGeodes();
            for( size_t j = 0; j < geodes.size(); ++j )
            {
                osg::ref_ptr< osg::Uniform > parExp =
                    geodes.at( j )->getDrawable( 0 )->getStateSet()->getUniform( "particleExp" );
                if( parExp.valid() )
                {
                    parExp->set( static_cast< float >( glow ) );
                }
            }
        }
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
