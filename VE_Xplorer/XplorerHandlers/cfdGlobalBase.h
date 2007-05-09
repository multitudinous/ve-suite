/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
#ifndef CFD_GLOBAL_BASE_H
#define CFD_GLOBAL_BASE_H
/*!\file cfdGlobalBase.h
cfdGlobalBase API
*/
/*!\class VE_Xplorer::cfdGlobalBase
* 
*/

namespace VE_Xplorer
{
   class cfdCommandArray;
}

namespace VE_XML
{
   class Command;
}

#include "VE_Installer/include/VEConfig.h"

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS cfdGlobalBase
{
public:
   cfdGlobalBase();
   virtual ~cfdGlobalBase ();
   ///copy constructor
   cfdGlobalBase( const cfdGlobalBase& ){;}

   ///this abstract base class declares some pure virtual int functions to be
   ///specified in concrete implementations

   ///compare VjObs_i commandArray with its child's value
   virtual bool CheckCommandId( VE_Xplorer::cfdCommandArray * _cfdCommandArray ) = 0;

   ///in future, multi-threaded apps will make a copy of VjObs_i commandArray
   virtual void UpdateCommand() = 0;

   ///Accessor to set the VECommand to be used in any class within Xplorer
   ///\param command holds the current command to be executed
   void SetVECommand( VE_XML::Command* command );

   ///Get the current command
   VE_XML::Command* GetVECommand();
protected:


   VE_Xplorer::cfdCommandArray* _cfdCommandArray;///<cfdApp side variables declared in VjObs_i.h

   VE_XML::Command* veCommand;///<cfdApp side variables declared in VjObs_i.h

private:
};
}
#endif
