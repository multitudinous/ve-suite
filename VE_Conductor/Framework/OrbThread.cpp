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
 *************** <auto-copyright.pl END do not edit this line> ***************/
// OrbThread.cpp: implementation of the OrbThread class.
//
//////////////////////////////////////////////////////////////////////
#include "OrbThread.h"
#include "Frame.h"
#include "Network.h"
//#include "ace/OS.h"
//#include <ace/Task.h>
#include <ace/OS.h>
#include <tao/BiDir_GIOP/BiDirGIOP.h>
#include <iostream>
#include <sstream>

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
/*
OrbThread::OrbThread(AppFrame* frame)
//: wxThread(wxTHREAD_JOINABLE)
{
   frame_ = frame;
   //Create();
}

OrbThread::~OrbThread()
{
}

//bool OrbThread::Do() 
int OrbThread::svc (void)
{
      try
      {
   long id = time(NULL);
   //char* uiname;
   //sprintf(uiname, "UIClient%ld", id);
   std::ostringstream dirStringStream;
   dirStringStream << "UIClient" << id;
   std::string UINAME = dirStringStream.str();
   //uiname = (char*)dirString.c_str();
   //std::string UINAME = uiname;
   CosNaming::Name name(1);
   name.length(1);
   name[0].id = CORBA::string_dup ("Executive");

   CORBA::Object_var naming_context_object =
      frame_->orb->resolve_initial_references ("NameService");
   CosNaming::NamingContext_var naming_context = CosNaming::NamingContext::_narrow (naming_context_object.in ());
   
   CORBA::Object_var exec_object = naming_context->resolve(name);
   frame_->network->exec = Body::Executive::_narrow(exec_object.in());

   //Create the Servant
   if (frame_->p_ui_i==NULL)
   {
      frame_->p_ui_i= new Body_UI_i(frame_->network->exec.in(), UINAME);

      //pass the Frame's pointer to the UI corba implementation
      frame_->p_ui_i->SetUIFrame(frame_);
      //Here is the code to set up the ROOT POA
      CORBA::Object_var poa_object =
      frame_->orb->resolve_initial_references ("RootPOA"); // get the root poa
      frame_->poa_root = PortableServer::POA::_narrow(poa_object.in());
      PortableServer::POAManager_var poa_manager = frame_->poa_root->the_POAManager ();

      CORBA::PolicyList policies (1);
      policies.length (1);

      CORBA::Any pol;
      pol <<= BiDirPolicy::BOTH;
      policies[0] =
         frame_->orb->create_policy (BiDirPolicy::BIDIRECTIONAL_POLICY_TYPE,
                            pol);

      // Create POA as child of RootPOA with the above policies.  This POA
      // will receive request in the same connection in which it sent
      // the request
      try
      {
         frame_->poa = frame_->poa_root->create_POA ("childPOA",
                              poa_manager.in (),
                              policies);
      }
      catch (const PortableServer::POA::AdapterAlreadyExists & )
      {
         std::cout << " Child POA Already Connected : Do nothing " << std::endl;
      }

      // Creation of childPOA is over. Destroy the Policy objects.
      for (CORBA::ULong i = 0; i < policies.length (); ++i)
      {
         policies[i]->destroy ();
      }

      poa_manager->activate();
      PortableServer::ObjectId_var id = PortableServer::string_to_ObjectId (CORBA::string_dup (UINAME.c_str()));
      frame_->poa->activate_object_with_id (id.in (),
                                          frame_->p_ui_i);

      //Activate it to obtain the object reference
      Body::UI_var ui = Body::UI::_narrow( frame_->poa->id_to_reference( id.in() ) );
    
      //CosNaming::Name UIname(1);
      //UIname.length(1);
      //UIname[0].id = CORBA::string_dup (UINAME.c_str());
   
      //Bind the object
      //try   {
      //      frame_->naming_context->bind(UIname, ui.in());
      //   }catch(CosNaming::NamingContext::AlreadyBound& ex){
      //      frame_->naming_context->rebind(UIname, ui.in());
      //   }
   
      //(frame_->_mutex).release();
      try {
         frame_->network->exec->RegisterUI(frame_->p_ui_i->UIName_.c_str(), ui.in());
         frame_->con_menu->Enable(v21ID_SUBMIT,true);
         frame_->con_menu->Enable(v21ID_LOAD, true);
         //frame_->con_menu->Enable(v21ID_CONNECT, false);
         frame_->run_menu->Enable(v21ID_VIEW_RESULT, true);
         frame_->con_menu->Enable(v21ID_DISCONNECT, true);
         
         frame_->orb->run();
      }catch (CORBA::Exception &) {
        
         frame_->Log("Can't find executive or UI registration error.\n");
      }
   }
   else
   {
      try {
         PortableServer::ObjectId_var id = PortableServer::string_to_ObjectId (CORBA::string_dup (UINAME.c_str()));

         //Activate it to obtain the object reference
         Body::UI_var ui = Body::UI::_narrow(frame_->poa->id_to_reference( id.in() ) );

         frame_->network->exec->RegisterUI(frame_->p_ui_i->UIName_.c_str(), ui.in());
         frame_->con_menu->Enable(v21ID_SUBMIT,true);
         frame_->con_menu->Enable(v21ID_LOAD, true);
         frame_->con_menu->Enable(v21ID_CONNECT, false);
         frame_->run_menu->Enable(v21ID_VIEW_RESULT, true);
         frame_->con_menu->Enable(v21ID_DISCONNECT, true);
       
      }catch (CORBA::Exception &) {
      
         frame_->Log("Can't find executive or UI registration error.\n");
      }
   }
      }catch (CORBA::Exception &) {
        
         frame_->Log("Can't find executive or UI registration error.\n");
      }
   
   return true;
}
*/
PEThread::PEThread(AppFrame* frame)
//: wxThread(wxTHREAD_JOINABLE)
{
   frame_ = frame;
   //Create();
   shutdown = true;
}

PEThread::~PEThread()
{
   ShutDownThread();
   ACE_OS::sleep(3); 
}

//bool PEThread::Do() 
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
         std::cout<<"LOG: "<<message<<std::endl;
         ::wxPostEvent(frame_, u);
         message="";
      }
      _mutex.release();
      ACE_OS::sleep(1); 
   }
   return 1;
}
/////////////////////////////////////////////////////////////
void PEThread::ShutDownThread( void )
{
   shutdown = false;
}
/////////////////////////////////////////////////////////////
void PEThread::SetMessage(const char* msg)
{
  _mutex.acquire();
  message+=msg;
  _mutex.release();
}
