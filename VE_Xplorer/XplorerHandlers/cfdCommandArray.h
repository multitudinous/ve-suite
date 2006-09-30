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
#ifndef CFD_COMMAND_ARRAY_H
#define CFD_COMMAND_ARRAY_H
/*!\file cfdCommandArray.h
cfdCommandArray API
*/
/*!\class VE_XPlorer::cfdCommandArray
* 
*/
// cfdApp side variables declared in VjObs_i.h
#include "VE_Installer/include/VEConfig.h"

namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS cfdCommandArray
   {
      public:
         cfdCommandArray();
         ~cfdCommandArray(){}
         cfdCommandArray( const cfdCommandArray& );
         cfdCommandArray& operator=( const cfdCommandArray& );

         double GetCommandValue( int );
         void SetCommandValue( int, double );

         enum commandArrayValue 
         { 
            CFD_ISO_VALUE,
            CFD_SC,
            CFD_MIN,
            CFD_MAX,
            CFD_ID,
            CFD_GEO_STATE,
            CFD_POSTDATA_STATE,
            CFD_PRE_STATE,
            CFD_TIMESTEPS,
            CFD_TEACHER_STATE 
         };

      private:

         double   cfdIso_value;
         double   cfdSc;
         double   cfdMin;
         double   cfdMax;
         double   cfdId;
         double   cfdGeo_state;
         double   cfdPostdata_state;
         double   cfdPre_state;
         double   cfdTimesteps; 
         double   cfdTeacher_state; 

   };
}
#endif

