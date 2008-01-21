/*************** <auto-copyright.pl BEGIN do not edit this line> **************
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CE_UTILITIES_OPORT_H
#define CE_UTILITIES_OPORT_H
#include <ves/VEConfig.h>
#include <ves/ce/util/Port.h>
#include <ves/open/moduleS.h>

namespace ves
{
namespace open
{
namespace xml
{
class Command;
}
}
}

namespace VE_CE
{
namespace Utilities
{
///Output port class
class VE_CE_UTILS_EXPORTS OPort : public Port
{
public:
    OPort( int, Module* );
    OPort( const OPort& );
    virtual ~OPort();

    void copy( const OPort& );

    int have_data();
    int have_profile();

    ///Get ouput port data
    ves::open::xml::Command* GetPortData( void );
    ///Set port data for respective port
    void SetPortData( ves::open::xml::Command* inputData );

    //Interface      _data;
    Types::Profile *_profile;
protected:
    ves::open::xml::Command* data;
};
}
}
#endif
