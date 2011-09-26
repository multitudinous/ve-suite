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

#pragma once

#include <string>

#include <ves/xplorer/volume/cfdTextureDataSet.h>

namespace ves
{
namespace xplorer
{
namespace event
{
namespace volume
{
///Play, stop, step an animation
void SetTransientMode( std::string const& playMode, std::string const& direction );

///Set the duration of the animation
void SetTransientDuration( double const& duration );

///
void EnablePhoneShader( bool const& enable );

///
void SetActiveShaderManager( std::string const& activeShaderManager );

///
void UpdateNumberOfSlicePlanes( unsigned int const& numberOfSlices );

///
void EnablePreIntegration( bool const& enable );

///
void UpdateIsoSurfaceValue( double const& value );

///
void EnableIsoSurfaces( bool const& enable );

///
void UpdateClipPlaneSettings( std::string const& planeDirection, 
    std::string const& planeCoordinate, double const& roiValue, 
    double const& minRoiValue, double const& maxRoiValue );

///
void TurnOnBBox( bool const& enable );

///
void ActivateTBDataset( std::string const& activeDataset );

///
void UpdateTBSolution( std::string const& dataName, std::string const& dataType, 
    double const& minRange, double const& maxRange );
    
///Update the current scalar range
void UpdateScalarRange( double const& minRange, double const& maxRange );

///Get the set active texture dataset. We assume there is only one texture
///dataset per model.
ves::xplorer::volume::cfdTextureDataSet* SetActiveTextureDataset();

} // namespace cad
} // namespace event
} // namespace xplorer
} // namespace ves