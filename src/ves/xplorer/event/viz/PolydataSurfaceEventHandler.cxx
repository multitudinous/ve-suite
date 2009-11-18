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
    ves::open::xml::CommandPtr command = 
        boost::dynamic_pointer_cast<ves::open::xml::Command>( veXMLObject );
    ves::open::xml::DataValuePairPtr opacityDVP = 
        command->GetDataValuePair( "opacity" );
    ves::open::xml::DataValuePairPtr warpScaleDVP = 
        command->GetDataValuePair( "warpScale" );
    ves::open::xml::DataValuePairPtr minValueDVP = 
        command->GetDataValuePair( "minValue" );
    ves::open::xml::DataValuePairPtr maxValueDVP = 
        command->GetDataValuePair( "maxValue" );

    std::vector< ves::xplorer::cfdGraphicsObject* > graphicsObject =
        ves::xplorer::SteadyStateVizHandler::instance()->
        GetGraphicsObjectsOfType( POLYDATA );
    
    if( graphicsObject.empty() )
    {
        return;
    }

    //Setup and change the warping scale
    if( warpScaleDVP )
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
    
    //Setup and change the opacity
    if( opacityDVP )
    {
        double opacityVal = 0;
        opacityDVP->GetData( opacityVal );
        opacityVal *= 0.01;
        for( size_t i = 0; i < graphicsObject.size(); ++i )
        {
            std::vector< osg::ref_ptr< ves::xplorer::scenegraph::Geode > > 
                geodes = graphicsObject.at( i )->GetGeodes();
            for( size_t j = 0; j < geodes.size(); ++j )
            {
                osg::ref_ptr< osg::Uniform > warpScaleUniform =
                    geodes.at( j )->getDrawable( 0 )->
                    getStateSet()->getUniform( "opacityVal" );
                if( warpScaleUniform.valid() )
                {
                    warpScaleUniform->set( static_cast< float >( opacityVal ) );
                    if( opacityVal < 1.0 )
                    {
                        geodes.at( j )->getDrawable( 0 )->
                            getStateSet()->
                            setRenderBinDetails( 10, "DepthSortedBin" );
                        geodes.at( j )->getDrawable( 0 )->
                            getStateSet()->setNestRenderBins( false );
                    }   
                    else
                    {
                        geodes.at( j )->getDrawable( 0 )->
                            getStateSet()->
                            setRenderBinDetails( 0, "RenderBin" );
                        geodes.at( j )->getDrawable( 0 )->
                            getStateSet()->setNestRenderBins( true );
                    }
                }
            }
        }
    }
    
    //Setup scalar control
    if( minValueDVP || maxValueDVP )
    {
        double opacityVal = 0;
        for( size_t i = 0; i < graphicsObject.size(); ++i )
        {
            std::vector< osg::ref_ptr< ves::xplorer::scenegraph::Geode > > 
            geodes = graphicsObject.at( i )->GetGeodes();
            for( size_t j = 0; j < geodes.size(); ++j )
            {
                osg::ref_ptr< osg::Uniform > warpScaleUniform =
                    geodes.at( j )->getDrawable( 0 )->
                    getStateSet()->getUniform( "scalarMinMax" );
                if( warpScaleUniform.valid() )
                {
                    osg::Vec2 opacityValVec;
                    warpScaleUniform->get( opacityValVec );
                    if( minValueDVP )
                    {
                        minValueDVP->GetData( opacityVal );
                        opacityValVec[ 0 ] = opacityVal;
                    }
                    else
                    {
                        maxValueDVP->GetData( opacityVal );
                        opacityValVec[ 1 ] = opacityVal;
                    }
                    warpScaleUniform->set( opacityValVec );
                }
            }
        }
    }
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
