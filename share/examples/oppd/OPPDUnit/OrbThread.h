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
//#include <wx/wx.h>
//#include <wx/thread.h>
#include "OPPDUnit_i.h"
#include "moduleC.h"
#include "orbsvcs/CosNamingC.h"
#include <ace/Task.h>
#include "moduleS.h"
#include <string>

class AppFrame;

class OrbThread : public ACE_Task_Base //wxThread  
{
public:
	OrbThread();
	virtual ~OrbThread();
	virtual int svc (void);
	//bool Do();
	//virtual ExitCode Entry() { return (ExitCode) this->Do(); };
protected:
	//CORBA::ORB_var _orb;
};

#endif // !defined(AFX_ORBTHREAD_H__1A5E5D0F_8D34_4791_87BC_3C2CEB837A2F__INCLUDED_)
