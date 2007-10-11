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
#ifndef CFD_COMMAND_ARRAY_H
#define CFD_COMMAND_ARRAY_H
/*!\file cfdCommandArray.h
cfdCommandArray API
*/
/*!\class VE_Xplorer::cfdCommandArray
* 
*/
// cfdApp side variables declared in VjObs_i.h
#include <ves/VEConfig.h>

namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS cfdCommandArray
   {
      public:
         ///Base constructor
         cfdCommandArray();
         ///destructor
         ~cfdCommandArray(){}
         ///copy constructor
         cfdCommandArray( const cfdCommandArray& );
         ///equal operator
         cfdCommandArray& operator=( const cfdCommandArray& );
         
         ///Get the Command Value 
         ///\param commandValue
         double GetCommandValue( int commandValue);
      
         ///Set the Command Value
         ///\param commandValue
         ///\param outputValue
         void SetCommandValue( int commandValue, double outputValue);

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

         double   cfdIso_value;///<The isosurface value.
         double   cfdSc;///<The scale value.
         double   cfdMin;///<The minimum value.
         double   cfdMax;///<The maximum value.
         double   cfdId;///<The ID.
         double   cfdGeo_state;///<The geometry state.
         double   cfdPostdata_state;///<The postdata state.
         double   cfdPre_state;///<The precomputed surface value.
         double   cfdTimesteps; ///<The timestep value.
         double   cfdTeacher_state; ///<The teacher state.

   };
}
#endif

