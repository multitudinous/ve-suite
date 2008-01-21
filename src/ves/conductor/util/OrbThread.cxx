/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
// OrbThread.cpp: implementation of the OrbThread class.
//
//////////////////////////////////////////////////////////////////////
#include <ves/conductor/util/OrbThread.h>

#include <ace/OS.h>
#include <iostream>
#include <sstream>
using namespace ves::conductor::util;

BEGIN_EVENT_TABLE( PEThread, wxTextCtrl )
    //EVT_UPDATE_UI(7777, PEThread::OnUpdateUIPop)
END_EVENT_TABLE()

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
PEThread::PEThread()
{
    //frame_ = frame;
    //Create();
    shutdown = true;
    //logWindow = new wxTextCtrl(wx_log_splitter, MYLOG, "", wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxTE_READONLY);
}

PEThread::~PEThread()
{
    ShutDownThread();
    ACE_OS::sleep( 1 );
}
////////////////////////////////////////////////////////////////////////////////
int PEThread::svc( void )
{
    while( shutdown )
    {
        _mutex.acquire();
        if (( message != "" ) )//&& (this->GetParent()))
        {
            //wxUpdateUIEvent u;
            //u.SetId(7777);
            //u.SetText(message.c_str());
            this->AppendText( wxString( message.c_str(), wxConvUTF8 ) );
            std::cout << "LOG: " << message;
            //::wxPostEvent(this, u);
            //message="";
        }
        _mutex.release();
        ACE_OS::sleep( 1 );
    }
    return 1;
}
////////////////////////////////////////////////////////////////////////////////
void PEThread::ShutDownThread( void )
{
    shutdown = false;
}
////////////////////////////////////////////////////////////////////////////////
void PEThread::SetMessage( const char* msg )
{
    _mutex.acquire();
    //message+=msg;
    this->AppendText( wxString( msg, wxConvUTF8 ) );
    _mutex.release();
}

