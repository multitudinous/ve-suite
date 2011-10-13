/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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

#include <ves/xplorer/eventmanager/EventManager.h>
#include <ves/xplorer/eventmanager/SlotWrapper.h>

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

    CONNECTSIGNALS_STATIC( "%TBETUpdateNumberOfSlicePlanes",
                      void( unsigned int const& ),
                      &UpdateNumberOfSlicePlanes,
                      m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_STATIC( "%TBETEnablePreIntegration",
                          void( bool const& ),
                          &EnablePreIntegration,
                          m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_STATIC( "%TBETUpdateIsoSurfaceValue",
                      void( double const& ),
                      &UpdateIsoSurfaceValue,
                      m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_STATIC( "%TBETEnableIsoSurfaces",
                      void( bool const& ),
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
                          
    /*CONNECTSIGNALS_STATIC( "%TB_SET_ACTIVE_SHADER_MANAGER",
                      void( const std::string&, const std::string&,
                            const std::string&),
                      &DeleteCADNode,
                      m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_STATIC( "%TB_ACTIVATE",
                      void( const std::string&, const double& ),
                      &SetMassOnCADNode,
                      m_connections, any_SignalType, normal_Priority );*/
}

} // namespace cad
} // namespace event
} // namespace xplorer
} // namespace ves
