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
#ifndef VE_XPLORER_GLOBAL_BASE_H
#define VE_XPLORER_GLOBAL_BASE_H

#include <ves/VEConfig.h>
#include <ves/xplorer/GlobalBasePtr.h>

#include <ves/open/xml/CommandPtr.h>

namespace ves
{
namespace xplorer
{

/*!\file GlobalBase.h
GlobalBase API
*/
/*!\class ves::xplorer::GlobalBase
*
*/
class VE_XPLORER_EXPORTS GlobalBase
{
public:
    GlobalBase();
    virtual ~GlobalBase();
    ///copy constructor
    GlobalBase( const GlobalBase& )
    {
        ;
    }

    ///this abstract base class declares some pure virtual int functions to be
    ///specified in concrete implementations
    ///Process the ves::open::xml::Command that has been passed in
    virtual void ProcessCommand()
    {
        ;
    }

    ///in future, multi-threaded apps will make a copy of VjObs_i commandArray
    virtual void UpdateCommand() = 0;

    ///Accessor to set the VECommand to be used in any class within Xplorer
    ///\param command holds the current command to be executed
    void SetVECommand( const ves::open::xml::CommandPtr& command );

    ///Get the current command
    const ves::open::xml::CommandPtr& GetVECommand();
protected:

    ves::open::xml::CommandPtr veCommand;///<cfdApp side variables declared in VjObs_i.h

private:
};
}
}
#endif
