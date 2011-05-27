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
#ifndef VES_XPLORER_EVENT_DATA_SLOTS_H
#define VES_XPLORER_EVENT_DATA_SLOTS_H

#include <ves/VEConfig.h>
#include <string>
#include <vector>

namespace ves
{
namespace xplorer
{
namespace event
{
namespace data
{
/*!\file DataSlots.h ves/xplorer/event/data/DataSlots.h
 *   Class for changing data signals
 * \namespace ves::xplorer::event::data
 */
/// Turns greyscaling for contour planes on or off.
void SetContourPlaneGreyscale( std::string const& uuid, std::vector< bool > const& greyscaleflag );

/// Transforms a data set in the scenegraph
void TransformDatasetNode( const std::string& uuid, const std::vector< double >& transform );

/// Toggles surface wrap for a data set
void SetDatasetSurfaceWrap( std::string const& uuid, bool const& surfaceWrap );

    
} //end data
} //end event
} //end xplorer
} //end ves

#endif //VES_XPLORER_EVENT_DATA_SLOTS_H
