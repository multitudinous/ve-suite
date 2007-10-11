/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
#ifndef COMMAND_HANDLER_H
#define COMMAND_HANDLER_H
/*!\file CommandHandler.h
*/
/*!\class VE_Xplorer::CommandHandler
* 
*/
#include "VE_Installer/include/VEConfig.h"

#include "VE_Open/XML/Command.h"

#include "VE_Xplorer/XplorerHandlers/Xplorer_i.h"

#include <vpr/Util/Singleton.h>

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS CommandHandler 
{
private:
	CommandHandler( void );
	~CommandHandler( void ){ ; }
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
   bool SetXMLCommand( VE_XML::Command* inputCommand );

   ///Get command from Xplorer
   VE_XML::Command* GetXMLCommand( void );

   ///
   void SetXplorer( Body_VEXplorer_i* xplorer );
   ///Send a text message to all conductors
   void SendConductorMessage( std::string message );

private:
	VE_XML::Command* m_input;///<XML command

	Body_VEXplorer_i* m_xplorer;
};
}
#endif //COMMAND_HANDLER_H
