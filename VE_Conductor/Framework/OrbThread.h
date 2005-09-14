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
// OrbThread.h: interface for the OrbThread class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_ORBTHREAD_H__1A5E5D0F_8D34_4791_87BC_3C2CEB837A2F__INCLUDED_)
#define AFX_ORBTHREAD_H__1A5E5D0F_8D34_4791_87BC_3C2CEB837A2F__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

//#ifdef WIN32
//#include <winsock2.h>
//#endif
//#include <wx/thread.h>
#include <ace/Task.h>
#include "moduleS.h"
#include <string>
#include <wx/menu.h>

class AppFrame;



class OrbThread : public ACE_Task_Base //wxThread  
{
public:
	OrbThread(AppFrame* frame);
	virtual ~OrbThread();
	virtual int svc (void);
	//bool Do();
	//virtual ExitCode Entry() { return (ExitCode) this->Do(); };
protected:
	AppFrame *frame_;
};


class PEThread : public ACE_Task_Base //wxThread  
{
public:
	PEThread(AppFrame* frame);
	virtual ~PEThread();
	virtual int svc (void);
	void SetMessage(const char* msg);
	//bool Do();
	//virtual ExitCode Entry() { return (ExitCode) this->Do(); };
protected:
	AppFrame *frame_;
	ACE_Thread_Mutex _mutex;
	std::string message;
};
#endif // !defined(AFX_ORBTHREAD_H__1A5E5D0F_8D34_4791_87BC_3C2CEB837A2F__INCLUDED_)
