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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_COMMAND_OBJECTS_H
#define CFD_COMMAND_OBJECTS_H
/*!\file cfdCommandObjects.h
cfdCommandObjects API
*/
/*!\class VE_Xplorer::cfdCommandObjects
* 
*/
#include "VE_Xplorer/XplorerHandlers/cfdGlobalBase.h"
namespace VE_Xplorer
{
   class cfdCommandArray;
}

namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS cfdCommandObjects : public cfdGlobalBase
   {
      public:
         ///Base Constructor
         cfdCommandObjects ();
         ///Destructor
         ~cfdCommandObjects ();

         // pure virtual int functions to be specified in concrete implementations

         ///compare VjObs_i commandArray with its child's value
         ///\param _cfdCommandArray
         virtual bool CheckCommandId( cfdCommandArray * _cfdCommandArray ) = 0;

         ///in future, multi-threaded apps will make a copy of VjObs_i commandArray
         virtual void UpdateCommand() = 0;

      protected:
      private:
   };
}
#endif

