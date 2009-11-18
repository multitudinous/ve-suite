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
#include <ves/xplorer/event/viz/PolydataSurfaceEventHandler.h>
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

using namespace ves::xplorer::event;

////////////////////////////////////////////////////////////////////////////////
PolydataSurfaceEventHandler::PolydataSurfaceEventHandler()
        :
        ves::xplorer::event::EventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
PolydataSurfaceEventHandler::PolydataSurfaceEventHandler( const PolydataSurfaceEventHandler& rhs )
        :
        ves::xplorer::event::EventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
PolydataSurfaceEventHandler::~PolydataSurfaceEventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void PolydataSurfaceEventHandler::SetGlobalBaseObject( ves::xplorer::GlobalBase* modelHandler )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void PolydataSurfaceEventHandler::Execute( const ves::open::xml::XMLObjectPtr& veXMLObject )
{
    ves::open::xml::CommandPtr command = boost::dynamic_pointer_cast<ves::open::xml::Command>( veXMLObject );
    ves::open::xml::DataValuePairPtr opacityDVP = command->GetDataValuePair( "opacity" );
    ves::open::xml::DataValuePairPtr warpScaleDVP = command->GetDataValuePair( "warpScale" );
    ves::open::xml::DataValuePairPtr minMaxDVP = command->GetDataValuePair( "minMax" );
    /*
    {
        osg::ref_ptr< osg::Uniform > surfaceWarpUniform =
        new osg::Uniform( "surfaceWarpScale", m_surfaceWarpScale );
        ss->addUniform( surfaceWarpUniform.get() );
    }
    
    {
        // Pass the min/max for the scalar range into the shader as a uniform.
        osg::Vec2s ts( dataRange[ 0 ], dataRange[ 1 ] );//- (dataRange[ 1 ]*0.10) );
        osg::ref_ptr< osg::Uniform > scalarMinMaxUniform =
        new osg::Uniform( "scalarMinMax", osg::Vec2( (float)ts.x(), (float)ts.y() ) );
        ss->addUniform( scalarMinMaxUniform.get() );
    }
    */    
    std::vector< ves::xplorer::cfdGraphicsObject* > graphicsObject =
        ves::xplorer::SteadyStateVizHandler::instance()->
        GetGraphicsObjectsOfType( POLYDATA );

    if( warpScaleDVP && !graphicsObject.empty() )
    {
        double warpScale = 0;
        warpScaleDVP->GetData( warpScale );

        for( size_t i = 0; i < graphicsObject.size(); ++i )
        {
            std::vector< osg::ref_ptr< ves::xplorer::scenegraph::Geode > > 
                geodes = graphicsObject.at( i )->GetGeodes();
            for( size_t j = 0; j < geodes.size(); ++j )
            {
                osg::ref_ptr< osg::Uniform > warpScaleUniform =
                    geodes.at( j )->getDrawable( 0 )->
                    getStateSet()->getUniform( "surfaceWarpScale" );
                if( warpScaleUniform.valid() )
                {
                    warpScaleUniform->set( static_cast< float >( warpScale ) );
                }
            }
        }
    }
    /*
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
    */
}
////////////////////////////////////////////////////////////////////////////////
PolydataSurfaceEventHandler& PolydataSurfaceEventHandler::operator=( const PolydataSurfaceEventHandler& rhs )
{
    if( this != &rhs )
    {
        ves::xplorer::event::EventHandler::operator=( rhs );
    }

    return *this;
}
////////////////////////////////////////////////////////////////////////////////
