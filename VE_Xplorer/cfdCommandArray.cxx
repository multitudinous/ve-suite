/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
 * File:          $RCSfile: cfdCommandArray.cxx,v $
 * Date modified: $Date: 2004-05-18 15:44:18 -0500 (Tue, 18 May 2004) $
 * Version:       $Rev: 382 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "cfdCommandArray.h"
#include <iostream>

using namespace std;

int cfdCommandArray::GetCommandValue( int i )
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
      return (int)cfdCommandArray::cfdId;
   else if ( i == CFD_GEO_STATE )
      return (int)cfdCommandArray::cfdGeo_state;
   else if ( i == CFD_POSTDATA_STATE )
      return (int)cfdCommandArray::cfdPostdata_state;
   else if ( i == CFD_PRE_STATE )
      return (int)cfdCommandArray::cfdPre_state;
   else if ( i == CFD_TIMESTEPS )
      return (int)cfdCommandArray::cfdTimesteps;
   else if ( i == CFD_TEACHER_STATE )
      return (int)cfdCommandArray::cfdTeacher_state;
   else
   {
      cerr << "invalid argument(" << i << ") in "
           << "cfdCommandArray::GetCommandValue()" << endl;
      return 0;
   }
}

