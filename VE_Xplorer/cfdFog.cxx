/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: filename,v $
 * Date modified: $Date: date $
 * Version:       $Rev: 999999 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/


#include "cfdFog.h"
#include <iostream>
#include <Performer/pr/pfFog.h>

cfdFog::cfdFog( void )
{
   appFog = new pfFog(); 
   appFog->setColor( 0.6f, 0.6f, 0.6f);
}


cfdFog::~cfdFog( void )
{
   delete appFog;
}

void cfdFog::_turnOnFog( double dist)
{
/*   appFog->setRange(0, dist);
   appFog->setFogType(PFFOG_PIX_EXP2);
cout << " here 1 " << endl;
   pfEnable(PFEN_FOG);
cout << " here 2 " << endl;
   appFog->apply();
cout << " here 3 " << endl;
   pfOverride(PFSTATE_FOG | PFSTATE_ENFOG, PF_ON);
cout << " here 4 " << endl;*/
}

void cfdFog::_turnOffFog( void )
{
/*   pfDisable(PFEN_FOG);
   pfOverride(PFSTATE_FOG | PFSTATE_ENFOG, PF_OFF);*/
}
