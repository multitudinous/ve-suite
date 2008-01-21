/*************** <auto-copyright.rb BEGIN do not edit this line> **************
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef VE_XPLORER_COMMAND_HANDLER_H
#define VE_XPLORER_COMMAND_HANDLER_H
/*!\file CommandHandler.h
*/
/*!\class ves::xplorer::CommandHandler
*
*/
#include <ves/VEConfig.h>
#include <ves/xplorer/CommandHandlerPtr.h>

#include <ves/xplorer/Xplorer_i.h>

#include <ves/open/xml/CommandPtr.h>

#include <vpr/Util/Singleton.h>

namespace ves
{
namespace xplorer
{
class VE_XPLORER_EXPORTS CommandHandler
{
private:
    CommandHandler( void );
    ~CommandHandler( void )
    {
        ;
    }
    vprSingletonHeader( CommandHandler );

public:
    ///Initialize environment.
    void Initialize( void );

    ///Clean up environment.
    void CleanUp( void );

    ///Initialize scene.
    void InitScene( void );

    ///Pre frame update.
    void PreFrameUpdate( void );

    ///Set command from Xplorer
    ///\param inputCommand
    bool SetXMLCommand( ves::open::xml::Command* inputCommand );

    ///Get command from Xplorer
    ves::open::xml::Command* GetXMLCommand( void );

    ///
    void SetXplorer( Body_VEXplorer_i* xplorer );

    ///Send a text message to all conductors
    void SendConductorMessage( std::string message );

private:
    ves::open::xml::Command* m_input;///<XML command

    Body_VEXplorer_i* m_xplorer;
};
}
}
#endif //COMMAND_HANDLER_H
