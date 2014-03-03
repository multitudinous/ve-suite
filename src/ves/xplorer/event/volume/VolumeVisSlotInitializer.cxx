/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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

#include <ves/xplorer/event/volume/VolumeVisSlotInitializer.h>
#include <ves/xplorer/event/volume/VolumeVisSlotInitializerPtr.h>

#include <ves/xplorer/event/volume/VolumeVisSlots.h>

#include <switchwire/EventManager.h>
#include <switchwire/OptionalMacros.h>

namespace ves
{
namespace xplorer
{
namespace event
{
namespace volume
{

VolumeVisSlotInitializer::VolumeVisSlotInitializer()
{
    using namespace ves::xplorer::event::volume;

    CONNECTSIGNALS_STATIC( "%TBETNumberOfSlicePlanes",
                           void( std::string const&, std::vector< int > const& ),
                           &UpdateNumberOfSlicePlanes,
                           m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_STATIC( "%TBETEnablePreIntegration",
                           void( bool const& ),
                           &EnablePreIntegration,
                           m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_STATIC( "%TBETPhongShading",
                           void( std::string const&, std::vector< bool > const& ),
                           &EnablePhoneShader,
                           m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_STATIC( "%TBETIsosurfaceValue",
                           void( std::string const&, std::vector< double > const& ),
                           &UpdateIsoSurfaceValue,
                           m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_STATIC( "%TBETEnableIsosurface",
                           void( std::string const&, std::vector< bool > const& ),
                           &EnableIsoSurfaces,
                           m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_STATIC( "%TBETUpdateScalarRange",
                           void( double const&, double const& ),
                           &UpdateScalarRange,
                           m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_STATIC( "%TBETUpdateScalar",
                           void( std::string const&, std::string const&, double const&, double const& ),
                           &UpdateTBSolution,
                           m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_STATIC( "%TBETROIBoundUpdate",
                           void( std::string const&, std::vector< double > const& ),
                           &UpdateROIBounds,
                           m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_STATIC( "%TBETAnimationDuration",
                           void( std::string const&, std::vector< double > const& ),
                           &SetTransientDuration,
                           m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_STATIC( "%TBETAnimationControls",
                           void( std::string const&, std::vector< std::string > const& ),
                           &SetTransientMode,
                           m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_STATIC( "%TBETHideVizFeature",
                           void( std::string const&, std::vector< bool > const& ),
                           &HideVizFeature,
                           m_connections, any_SignalType, normal_Priority );

	CONNECTSIGNALS_STATIC( "%TBETUpdateLfxVtkPolyData",
                           void( bool, double, bool ),
                           &UpdateLfxVtkPolyData,
                           m_connections, any_SignalType, normal_Priority );

	CONNECTSIGNALS_STATIC( "%TBETUpdateLfxVtkVectorData",
                           void( double, double, double, double, int ),
                           &UpdateLfxVtkVectorData,
                           m_connections, any_SignalType, normal_Priority );

	CONNECTSIGNALS_STATIC( "%TBETUpdateLfxVtkStreamline",
                           void( const std::vector<double>&, const std::vector<int>&, int, float, float, float ),
                           &UpdateLfxVtkStreamline,
                           m_connections, any_SignalType, normal_Priority ); 

	CONNECTSIGNALS_STATIC( "%TBETUpdateLfxVtkScalarRange",
                           void( std::string const&, double, double  ),
                           &UpdateLfxVtkScalarRange,
                           m_connections, any_SignalType, normal_Priority );

	CONNECTSIGNALS_STATIC( "%TBETUpdateLfxVtkScalar",
                           void( std::string const&, std::string const&  ),
                           &UpdateLfxVtkScalar,
                           m_connections, any_SignalType, normal_Priority );

	CONNECTSIGNALS_STATIC( "%TBETUpdateLfxVtkVector",
                           void( std::string const&, std::string const&  ),
                           &UpdateLfxVtkVector,
                           m_connections, any_SignalType, normal_Priority );

	CONNECTSIGNALS_STATIC( "%TBETUpdateLfxVtkColorByScalar",
                           void( std::string const&, std::string const&  ),
                           &UpdateLfxVtkColorByScalar,
                           m_connections, any_SignalType, normal_Priority );

	

	CONNECTSIGNALS_STATIC( "%TBETUpdateLfxChannel",
                           void( std::string const&, std::string const&  ),
                           &UpdateLfxChannel,
                           m_connections, any_SignalType, normal_Priority );

	CONNECTSIGNALS_STATIC( "%TBETUpdateLfxRenderProp",
                           void( std::string const&, std::string const&, int, boost::any, boost::any  ),
                           &UpdateLfxRenderProp,
                           m_connections, any_SignalType, normal_Priority );
}

} // namespace cad
} // namespace event
} // namespace xplorer
} // namespace ves
