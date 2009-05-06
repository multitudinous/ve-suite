/*************** <auto-copyright.rb BEGIN do not edit this line> **************
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
 *************** <auto-copyright.rb END do not edit this line> ***************/

#ifndef GUI_ACTION_ADAPTER
#define GUI_ACTION_ADAPTER

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

// --- VRJuggler Includes --- //

// --- OSG Includes --- //
#include <osgGA/GUIActionAdapter>

namespace ves
{
namespace xplorer
{
namespace device
{

/*!\file GUIActionAdapter.h
 * GUIActionAdapter API
 */

/*!\class ves::xplorer::GUIActionAdapter
 *
 */

class VE_XPLORER_EXPORTS GUIActionAdapter : public osgGA::GUIActionAdapter
{
public:
    ///
    ~GUIActionAdapter();

    ///
    virtual void requestRedraw();

    ///
    virtual void requestContinuousUpdate( bool needed = true );

    ///
    virtual void requestWarpPointer( float x, float y );

protected:

private:

};
} //end device
} //end xplorer
} //end ves

#endif //GUI_ACTION_ADAPTER
