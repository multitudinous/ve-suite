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

#include <ves/xplorer/event/cfdCommandArray.h>
#include <iostream>

using namespace VE_Xplorer;

cfdCommandArray::cfdCommandArray( const cfdCommandArray& input )
{
   cfdIso_value      = input.cfdIso_value;
   cfdSc             = input.cfdSc;
   cfdMin            = input.cfdMin;
   cfdMax            = input.cfdMax;
   cfdId             = input.cfdId;
   cfdGeo_state      = input.cfdGeo_state;
   cfdPostdata_state = input.cfdPostdata_state;
   cfdPre_state      = input.cfdPre_state;
   cfdTimesteps      = input.cfdTimesteps;
   cfdTeacher_state  = input.cfdTeacher_state; 
}

cfdCommandArray::cfdCommandArray()
{
   cfdIso_value      = -1;
   cfdSc             = -1;
   cfdMin            = -1;
   cfdMax            = -1;
   cfdId             = -1;
   cfdGeo_state      = -1;
   cfdPostdata_state = 0;
   cfdPre_state      = 0;
   cfdTimesteps      = -1;
   cfdTeacher_state  = -1;
}

cfdCommandArray& cfdCommandArray::operator=( const cfdCommandArray& input )
{
   if ( this != &input )
   {
      cfdIso_value      = input.cfdIso_value;
      cfdSc             = input.cfdSc;
      cfdMin            = input.cfdMin;
      cfdMax            = input.cfdMax;
      cfdId             = input.cfdId;
      cfdGeo_state      = input.cfdGeo_state;
      cfdPostdata_state = input.cfdPostdata_state;
      cfdPre_state      = input.cfdPre_state;
      cfdTimesteps      = input.cfdTimesteps;
      cfdTeacher_state  = input.cfdTeacher_state; 
   }
   return *this;
}

double cfdCommandArray::GetCommandValue( int i )
{
   if      ( i == CFD_ISO_VALUE )
      return cfdCommandArray::cfdIso_value;
   else if ( i == CFD_SC )
      return cfdCommandArray::cfdSc;
   else if ( i == CFD_MIN )
      return cfdCommandArray::cfdMin;
   else if ( i == CFD_MAX )
      return cfdCommandArray::cfdMax;
   else if ( i == CFD_ID )
      return cfdCommandArray::cfdId;
   else if ( i == CFD_GEO_STATE )
      return cfdCommandArray::cfdGeo_state;
   else if ( i == CFD_POSTDATA_STATE )
      return cfdCommandArray::cfdPostdata_state;
   else if ( i == CFD_PRE_STATE )
      return cfdCommandArray::cfdPre_state;
   else if ( i == CFD_TIMESTEPS )
      return cfdCommandArray::cfdTimesteps;
   else if ( i == CFD_TEACHER_STATE )
      return cfdCommandArray::cfdTeacher_state;
   else
   {
      std::cerr << "invalid argument(" << i << ") in "
           << "cfdCommandArray::GetCommandValue()" << std::endl;
      return 0;
   }
}

void cfdCommandArray::SetCommandValue( int i, double value)
{
   if      ( i == CFD_ISO_VALUE )
   {
      cfdIso_value = value;
   }
   else if ( i == CFD_SC )
   {
      cfdSc = value;
   }
   else if ( i == CFD_MIN )
   {
      cfdMin = value;
   }
   else if ( i == CFD_MAX )
   {
      cfdMax = value;
   }
   else if ( i == CFD_ID )
   {
      cfdId = value;
   }
   else if ( i == CFD_GEO_STATE )
   {
      cfdGeo_state = value;
   }  
   else if ( i == CFD_POSTDATA_STATE )
   {
      cfdPostdata_state = value;
   }
   else if ( i == CFD_PRE_STATE )
   {
      cfdPre_state = value;
   }
   else if ( i == CFD_TIMESTEPS )
   {
      cfdTimesteps = value;
   }
   else if ( i == CFD_TEACHER_STATE )
   {
      cfdTeacher_state = value;
   }
   else
   {
      std::cerr << "invalid argument(" << i << ") in "
           << "cfdCommandArray::SetCommandValue()" << std::endl;
   }

}
