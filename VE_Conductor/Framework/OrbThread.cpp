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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
// OrbThread.cpp: implementation of the OrbThread class.
//
//////////////////////////////////////////////////////////////////////
#include "OrbThread.h"
#include "Frame.h"
#include "Network.h"

#include <wx/app.h>
#include <ace/OS.h>
#include <tao/BiDir_GIOP/BiDirGIOP.h>
#include <iostream>
#include <sstream>

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
PEThread::PEThread(AppFrame* frame)
{
   frame_ = frame;
   //Create();
   shutdown = true;
}

PEThread::~PEThread()
{
   ShutDownThread();
   ACE_OS::sleep(1); 
}
////////////////////////////////////////////////////////////////////////////////
int PEThread::svc (void)
{
   while( shutdown )
   {
      _mutex.acquire();
      if (message!="")
      {
         wxUpdateUIEvent u;
         u.SetId(7777);
         u.SetText(message.c_str());
         std::cout<<"LOG: "<<message;
         ::wxPostEvent(frame_, u);
         message="";
      }
      _mutex.release();
      ACE_OS::sleep(1); 
   }
   return 1;
}
////////////////////////////////////////////////////////////////////////////////
void PEThread::ShutDownThread( void )
{
   shutdown = false;
}
////////////////////////////////////////////////////////////////////////////////
void PEThread::SetMessage(const char* msg)
{
   ::wxWakeUpIdle();
   _mutex.acquire();
   message+=msg;
   _mutex.release();
}
