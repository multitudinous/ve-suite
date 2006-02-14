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
 * File:          $RCSfile: Network_Exec.h,v $
 * Date modified: $Date: 2006-01-10 11:21:30 -0600 (Tue, 10 Jan 2006) $
 * Version:       $Rev: 3470 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
////////////////////////////////////////////////////////////////////////////////
OPort::OPort (int id, Module* m)
  : Port (id, m)
{
  _profile = NULL;
}
////////////////////////////////////////////////////////////////////////////////
OPort::OPort (const OPort& p)
  : Port(p)
{
  copy(p);
}
////////////////////////////////////////////////////////////////////////////////
OPort::~OPort ()
{
}
////////////////////////////////////////////////////////////////////////////////
void OPort::copy (const OPort& p)
{
  if(this==&p) return;
  
  _data    = p._data;
  if(_profile) delete _profile;
  _profile = new Types::Profile(*(p._profile));
}
////////////////////////////////////////////////////////////////////////////////
int OPort::have_data ()
{
  return((_data.getInts()).size()      !=0 ||
	 (_data.getDoubles()).size()   !=0 ||
	 (_data.getStrings()).size()   !=0 ||
	 (_data.getInts1D()).size()    !=0 ||
	 (_data.getDoubles1D()).size() !=0 ||
	 (_data.getStrings1D()).size() !=0 );
}
////////////////////////////////////////////////////////////////////////////////
int OPort::have_profile ()
{
  return(_profile != NULL);
}

