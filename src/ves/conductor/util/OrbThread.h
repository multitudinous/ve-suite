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
#if !defined(AFX_ORBTHREAD_H__1A5E5D0F_8D34_4791_87BC_3C2CEB837A2F__INCLUDED_)
#define AFX_ORBTHREAD_H__1A5E5D0F_8D34_4791_87BC_3C2CEB837A2F__INCLUDED_
/*!\file OrbThread.h
PEThread API
*/

/*!\class PEThread
*
*/

#include <ace/Task.h>
#include <string>
#include <wx/menu.h>
#include <wx/textctrl.h>

class wxUpdateUIEvent;

#include "VE_Installer/include/VEConfig.h"
class VE_CONDUCTOR_UTILS_EXPORTS PEThread : public ACE_Task_Base, public wxTextCtrl
{
public:
	PEThread( void );
	virtual ~PEThread();
	void SetMessage(const char* msg);
   virtual int svc( void );
   void ShutDownThread( void );

protected:
	//wxTextCtrl* logWindow;
	ACE_Thread_Mutex _mutex;
	std::string message;
   bool shutdown;
DECLARE_EVENT_TABLE()
};
#endif // !defined(AFX_ORBTHREAD_H__1A5E5D0F_8D34_4791_87BC_3C2CEB837A2F__INCLUDED_)
