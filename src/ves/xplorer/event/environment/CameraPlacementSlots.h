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
#pragma once

#include <ves/VEConfig.h>

#include <string>
#include <vector>

namespace ves
{
namespace xplorer
{
namespace event
{
namespace environment
{
/*!\file CameraPlacementSlots.h ves/xplorer/event/environment/CameraPlacementSlots.h
 *   Class for changing camera signals
 * \namespace ves::xplorer::event::environment
 */

void DisableCameraTools( bool flag );
void AddCamera( const std::string &uuid, const std::string &name );
void RemoveCamera( const std::string& uuid );
void SelectCamera( const std::string &uuid );
void ChangeCameraName( const std::string& uuid, const std::string& newName );
void CameraManagerOn( bool flag );
void CameraWindowOn( bool flag );
void CameraWindowResolution( int resolution );
void PictureModeOn( bool flag );
void SaveCameraImage( const std::string& uuid );
void SaveAllCameraImages( );
void CameraAutoComputeFarPlane( const std::string& uuid, bool flag );
void ShowCameraGeometry( const std::string& uuid, bool flag );
void ShowCameraFrustumGeometry( const std::string& uuid, bool flag );
void CameraFocalDistance( const std::string& uuid, double focalDistance );
void CameraFocalRange( const std::string& uuid, double focalRange );
void CameraMaxCircleOfConfusion( const std::string& uuid, double maxCircle );
void CameraProjectionUpdate( const std::string& uuid );

} //end environment
} //end event
} //end xplorer
} //end ves
