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
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/Framework/Frame.h"
#include <wx/imaglist.h>
#include <wx/artprov.h>
#include "VE_Conductor/Framework/ResultPanel.h"
#include "VE_Conductor/Framework/App.h"
#include "VE_Conductor/Framework/package.h"
#include "VE_Conductor/Framework/OrbThread.h"
#include "VE_Conductor/Framework/Avail_Modules.h"
#include "VE_Conductor/Framework/FinancialDialog.h"
#include "VE_Conductor/Framework/TextResultDialog.h"
#include "VE_Conductor/Framework/TexTable.h"
#include "VE_Conductor/Framework/GlobalParamDialog.h"
#include "VE_Conductor/Framework/SummaryResultDialog.h"
#include "VE_Conductor/VE_UI/UI_Tabs.h"
#include "VE_Conductor/VE_UI/UI_Frame.h"
#include "VE_Conductor/Framework/Network.h"

#include <wx/image.h>
#include <wx/bitmap.h>
#include <wx/splash.h>
#include "VE_Installer/installer/installerImages/ve_ce_banner.xpm"
#include "VE_Installer/installer/installerImages/ve_xplorer_banner.xpm"
#include <sstream>
#include <iomanip>

BEGIN_EVENT_TABLE (AppFrame, wxFrame)
  EVT_CLOSE(AppFrame::OnClose)
  EVT_MENU(v21ID_ZOOMIN, AppFrame::ZoomIn)
  EVT_MENU(v21ID_ZOOMOUT, AppFrame::ZoomOut)
  EVT_MENU(wxID_SAVE, AppFrame::Save)
  EVT_MENU(wxID_SAVEAS, AppFrame::SaveAs)
  EVT_MENU(wxID_NEW, AppFrame::New)
   // this is probably a bug and needs to be fixed
  EVT_MENU(wxID_EXIT, AppFrame::FrameClose)
  EVT_MENU(wxID_OPEN, AppFrame::Open)
  EVT_MENU(v21ID_LOAD, AppFrame::LoadFromServer)
  EVT_MENU(v21ID_SUBMIT, AppFrame::SubmitToServer)
  EVT_MENU(v21ID_CONNECT, AppFrame::ConExeServer)
  EVT_MENU(v21ID_DISCONNECT, AppFrame::DisConExeServer)
  EVT_MENU(v21ID_DISCONNECT_VE, AppFrame::DisConVEServer)
  EVT_MENU(v21ID_CONNECT_VE, AppFrame::ConVEServer)
  EVT_MENU(v21ID_START_CALC, AppFrame::StartCalc)
  EVT_MENU(v21ID_STOP_CALC, AppFrame::StopCalc)
  EVT_MENU(v21ID_PAUSE_CALC, AppFrame::PauseCalc)
  EVT_MENU(v21ID_RESUME_CALC, AppFrame::ResumeCalc)
  EVT_MENU(v21ID_HELP, AppFrame::ViewHelp)
  EVT_MENU(v21ID_VIEW_RESULT, AppFrame::ViewResult)

//  EVT_MENU(v21ID_GLOBAL_PARAM, AppFrame::GlobalParam)
//  EVT_MENU(v21ID_BASE, AppFrame::LoadBase)
//  EVT_MENU(v21ID_SOUR, AppFrame::LoadSour)
//  EVT_MENU(v21ID_REI_BASE, AppFrame::LoadREIBase)
//  EVT_MENU(v21ID_REI_SOUR, AppFrame::LoadREISour)
  EVT_UPDATE_UI(7777, AppFrame::OnUpdateUIPop)
END_EVENT_TABLE()

AppFrame::AppFrame(wxWindow * parent, wxWindowID id, const wxString& title)
  :wxFrame(parent, id, title), m_frameNr(0)
{
  wx_log_splitter = new wxSplitterWindow(this, -1);
  wx_log_splitter->SetMinimumPaneSize( 40 );
  wx_ve_splitter = new wxSplitterWindow(wx_log_splitter, -1);
  wx_ve_splitter->SetMinimumPaneSize( 20 );
  wx_nw_splitter = new wxSplitterWindow(wx_ve_splitter, -1);
  wx_nw_splitter->SetMinimumPaneSize( 20 );
  
  //LogWindow
  logwindow = new wxTextCtrl(wx_log_splitter, MYLOG, "", wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxTE_READONLY);

  // VE Tabs
  //m_tabs = ( UI_Tabs*) NULL;
  m_frame = 0;
  is_orb_init= false;
  p_ui_i=NULL;	
  av_modules = new Avail_Modules(wx_nw_splitter, TREE_CTRL, wxDefaultPosition, wxDefaultSize, wxTR_HAS_BUTTONS);
  network = new Network(wx_nw_splitter, -1 );
  av_modules->SetNetwork(network);
	
  wx_log_splitter->SplitHorizontally(wx_ve_splitter, logwindow, -100);
  wx_nw_splitter->SplitVertically(av_modules, network, 140);
  wx_ve_splitter->Initialize(wx_nw_splitter);
  SetSize(DetermineFrameSize(NULL));
  CreateMenu();
  CreateStatusBar();
  SetStatusText("VE-Conductor Status");
  
  pelog = NULL;
  //  menubar = 
}

void AppFrame::CreateVETab()
{
  //create the image list for the tabs first
  // create a dummy image list with a few icons

  wxSize imageSize(32, 32);
  
  m_imageList = new wxImageList( imageSize.GetWidth(), imageSize.GetHeight() );
  
  m_imageList->Add(wxArtProvider::GetIcon(wxART_INFORMATION, wxART_OTHER, imageSize));
  
  m_imageList->Add(wxArtProvider::GetIcon(wxART_QUESTION, wxART_OTHER, imageSize));
  
  m_imageList->Add(wxArtProvider::GetIcon(wxART_WARNING, wxART_OTHER, imageSize));
  
  m_imageList->Add(wxArtProvider::GetIcon(wxART_ERROR, wxART_OTHER, imageSize));
  
   //m_tabs = new UI_Tabs(vjobs.in(), wx_ve_splitter, ID_UI_TABS);
   m_frame = new UI_Frame(vjobs.in(), wx_ve_splitter, wxID_HIGHEST);
   // Create the notebook's panels
   //m_tabs->AssignImageList(m_imageList);
   //m_frame->_tabs->AssignImageList(m_imageList);
   //m_tabs->createTabPages();
   //wxNotebookSizer *nbs = new wxNotebookSizer(m_tabs);
   sizerTab = new wxBoxSizer(wxVERTICAL);
   //sizerTab->Add(nbs, 1, wxEXPAND | wxALL);
   sizerTab->Add(m_frame, 1, wxEXPAND | wxALL);
   sizerTab->Layout();
 
   wx_ve_splitter->SetSizer(sizerTab);
   wx_ve_splitter->SetAutoLayout(TRUE);

   //wx_ve_splitter->SplitVertically(wx_nw_splitter, m_tabs, 0);
   wx_ve_splitter->SplitVertically(wx_nw_splitter, m_frame, 0);

   //trying to get the tabs to show up on initialization!!!!!
   //wxSize windowSize = m_tabs->GetSize();
   //m_tabs->SetSize(windowSize.GetWidth()+1,windowSize.GetHeight()+1);
   //m_tabs->Refresh();

   wxSize windowSize = m_frame->GetSize();
   m_frame->SetSize(windowSize.GetWidth()+1,windowSize.GetHeight()+1);
   m_frame->Refresh();
}

wxRect AppFrame::DetermineFrameSize (wxConfig* config)
{
  const int minFrameWidth = 600;
  const int minFrameHight = 400;
  
  wxRect rect;
  wxSize scr = wxGetDisplaySize();
  wxConfig* cfg = config;
  if (!config) cfg = new wxConfig (wxTheApp->GetAppName());
  int i;
  for (i = 0; i <= m_frameNr; i++) {
    wxString key = LOCATION + wxString::Format ("%d", m_frameNr - i);
    if (cfg->Exists (key)) {
      rect.x = cfg->Read (key + _T("/") + LOCATION_X, rect.x);
      rect.y = cfg->Read (key + _T("/") + LOCATION_Y, rect.y);
      rect.width = cfg->Read (key + _T("/") + LOCATION_W, rect.width);
      rect.height = cfg->Read (key + _T("/") + LOCATION_H, rect.height);
      break;
    }
  }
  if (!config) delete cfg;
  
  // check for reasonable values (within screen)
  rect.x = wxMin (abs (rect.x), (scr.x - minFrameWidth));
  rect.y = wxMin (abs (rect.y), (scr.y - minFrameHight));
  rect.width = wxMax (abs (rect.width), (minFrameWidth));
  rect.width = wxMin (abs (rect.width), (scr.x - rect.x));
  rect.height = wxMax (abs (rect.height), (minFrameHight));
  rect.height = wxMin (abs (rect.height), (scr.y - rect.y));
  
  return rect;
}

void AppFrame::StoreFrameSize (wxRect rect, wxConfig* config)
{
  // store size
  wxConfig* cfg = config;
  if (!config) cfg = new wxConfig (wxTheApp->GetAppName());
  wxString key = LOCATION + wxString::Format ("%d", m_frameNr);
  cfg->Write (key + _T("/") + LOCATION_X, rect.x);
  cfg->Write (key + _T("/") + LOCATION_Y, rect.y);
  cfg->Write (key + _T("/") + LOCATION_W, rect.width);
  cfg->Write (key + _T("/") + LOCATION_H, rect.height);
  if (!config) delete cfg;
}

void AppFrame::OnClose(wxCloseEvent& WXUNUSED(event) )
{
   if (is_orb_init)
   {
      //CosNaming::Name UIname(1);
      //UIname.length(1);
      //if (p_ui_i!=NULL && p_ui_i->UIName_!="")
	   //{
	   // UIname[0].id = CORBA::string_dup ((p_ui_i->UIName_).c_str());
	   //naming_context->unbind(UIname);
	   if ( !CORBA::is_nil( poa.in() ) )
	      poa->destroy (1, 1);
	   //}
      orb->destroy();
   }
  
   StoreFrameSize(GetRect(), NULL);
   Destroy();
}

void AppFrame::FrameClose(wxCommandEvent& WXUNUSED(event) )
{
   Close(true);
}

void AppFrame::CreateMenu() 
{
  menubar = new wxMenuBar;
  file_menu = new wxMenu;
  con_menu = new wxMenu;
  run_menu = new wxMenu;
  edit_menu = new wxMenu;
  help_menu = new wxMenu;
//  config_menu = new wxMenu;

  file_menu->Append(wxID_NEW, _("&New\tCtrl+N"));
  file_menu->Append(wxID_OPEN, _("&Open ..\tCtrl+O"));
  file_menu->Append(wxID_SAVE, _("&Save\tCtrl+S"));
  file_menu->Append(wxID_SAVEAS, _("Save &as ..\tCtrl+Shift+S"));
  file_menu->AppendSeparator();
  file_menu->Append (wxID_PRINT_SETUP, _("Print Set&up .."));
  file_menu->Append (wxID_PREVIEW, _("Print Pre&view\tCtrl+Shift+P"));
  file_menu->Append (wxID_PRINT, _("&Print ..\tCtrl+P"));
  file_menu->AppendSeparator();
  file_menu->Append (wxID_EXIT, _("&Quit\tCtrl+Q"));

  file_menu->Enable(wxID_PRINT_SETUP, false);
  file_menu->Enable(wxID_PREVIEW, false);	
  file_menu->Enable(wxID_PRINT, false);
  
  
  con_menu->Append(v21ID_CONNECT, _("&Connect to Executive\tCtrl+C"));
  con_menu->Append(v21ID_CONNECT_VE, _("Connect to VE"));
  con_menu->AppendSeparator();
  con_menu->Append(v21ID_SUBMIT, _("Sub&mit Job\tCtrl+M"));
  con_menu->Append(v21ID_LOAD, _("&Load Job\tCtrl+L"));
  con_menu->AppendSeparator();
  con_menu->Append(v21ID_DISCONNECT, _("&Disconnect\tCtrl+d"));
  con_menu->Append(v21ID_DISCONNECT_VE, _("&Disconnect VE"));

  con_menu->Enable(v21ID_SUBMIT,false);
  con_menu->Enable(v21ID_LOAD, false);
  con_menu->Enable(v21ID_DISCONNECT, false);
  con_menu->Enable(v21ID_DISCONNECT_VE, false);

  
  run_menu->Append(v21ID_START_CALC, _("Start Simulation"));
  run_menu->Append(v21ID_STOP_CALC, _("Stop Simulation"));
  run_menu->Append(v21ID_PAUSE_CALC, _("Pause Simulation"));
  run_menu->Append(v21ID_RESUME_CALC, _("Resume Simulation"));
  run_menu->Append(v21ID_VIEW_RESULT, _("View Results"));
  // run_menu->Append(v21ID_GLOBAL_PARAM, _("Global Parameters"));
  // run_menu->Append(v21ID_VIEW_FINANCIAL, _("View Financial Params"));
  
  run_menu->Enable(v21ID_START_CALC, false);
  run_menu->Enable(v21ID_STOP_CALC, false);
  run_menu->Enable(v21ID_PAUSE_CALC, false);
  run_menu->Enable(v21ID_RESUME_CALC, false);
  // EPRI TAG run_menu->Enable(v21ID_VIEW_RESULT, false);
  
  edit_menu->Append(v21ID_UNDO, _("&Undo\tCtrl+U"));
  edit_menu->Append(v21ID_REDO, _("&Redo\tCtrl+R"));
  edit_menu->AppendSeparator();
  edit_menu->Append(v21ID_ZOOMIN, _("Zoom &In\tCtrl+I"));
  edit_menu->Append(v21ID_ZOOMOUT, _("&Zoom Out\tCtrl+Z"));
  
  edit_menu->Enable(v21ID_UNDO, false);
  edit_menu->Enable(v21ID_REDO, false);
   
  help_menu->Append(wxID_HELP_CONTENTS, _("&Content\tF1"));
  help_menu->Append (v21ID_HELP, _("&Index"));
  help_menu->AppendSeparator();
  help_menu->Append (wxID_ABOUT, _("&About ..\tShift+F1"));

  help_menu->Enable(wxID_HELP_CONTENTS, false);
  //help_menu->Enable(v21ID_HELP, false);
  help_menu->Enable(wxID_ABOUT, false);

//  config_menu->Append(v21ID_BASE,_("Base Quench"));
//  config_menu->Append(v21ID_SOUR, _("Base Quench & Sour Shift CO2"));
//  config_menu->Append(v21ID_REI_BASE, _("Base Quench (REI)"));
//  config_menu->Append(v21ID_REI_SOUR, _("Base Quench & Sour Shift CO2 (REI)"));
//  config_menu->Append(v21ID_SWEET, _("Sweet Shift CO2"));
//  config_menu->Append(v21ID_CO_DISPOSAL, _("Co-Disposal of H2S+CO2"));
  
//  config_menu->Enable(v21ID_SWEET, false);
//  config_menu->Enable(v21ID_CO_DISPOSAL, false);

  menubar->Append(file_menu, _("&File"));
//  menubar->Append(config_menu, _("&Configurations"));
  menubar->Append(edit_menu, _("&Edit"));
  menubar->Append(con_menu, _("&Connection"));
  menubar->Append(run_menu, _("&Execution"));
  menubar->Append(help_menu, _("&Help"));

  SetMenuBar(menubar);
}

void AppFrame::ZoomIn(wxCommandEvent& WXUNUSED(event) )
{
  //  printf("Scale = %g\n", network->m_xUserScale);
  int w, h, sx, sy;
  int xpos, ypos;
  network->GetClientSize(&w, &h);
  //  printf("Client size w: %d, h: %d\n", w, h);
  network->GetVirtualSize(&sx, &sy);
  //  printf("Virtual size sx: %d, sy: %d\n", sx, sy);

  if (network->m_xUserScale>4)
    return; // maximum zoom in x3

  network->m_xUserScale+=0.1;
  network->m_yUserScale+=0.1; 

  
  network->nPixX += 1;
  network->nPixY += 1;
  network->GetViewStart(&xpos, &ypos);
  network->SetScrollbars( network->nPixX, network->nPixY, network->nUnitX, network->nUnitY );
  network->Scroll(xpos, ypos);
  network->ReDrawAll();
}

void AppFrame::ZoomOut(wxCommandEvent& WXUNUSED(event))
{
  //  printf("Scale = %g\n", network->m_xUserScale);
  int w, h, sx, sy;
  int xpos, ypos;
  network->GetClientSize(&w, &h);
  //  printf("Client size w: %d, h: %d\n", w, h);
  network->GetVirtualSize(&sx, &sy);
  //  printf("Virtual size sx: %d, sy: %d\n", sx, sy);

  if (network->m_xUserScale<0.4)
    return; //minimum x-5

  network->m_xUserScale-=0.1;
  network->m_yUserScale-=0.1;
  network->nPixX-=1;
  network->nPixY-=1;

  network->GetViewStart(&xpos, &ypos);
  network->SetScrollbars( network->nPixX, network->nPixY, network->nUnitX, network->nUnitY );
  network->Scroll(xpos, ypos);
  network->ReDrawAll();
}

void AppFrame::Save(wxCommandEvent& event)
{
  if (path==wxString("")) //First time call save will be the same as SaveAs
    SaveAs(event);
  else
    network->Save(path);
}

void AppFrame::SaveAs(wxCommandEvent& WXUNUSED(event))
{
  wxFileDialog dialog
    (
     this,
     _T("Save File dialog"),
     directory,
     fname,
     _T("Network files (*.nt)|*.nt"),
	 wxSAVE|wxOVERWRITE_PROMPT 
     );
  
  if (directory=="")
    dialog.SetDirectory(wxGetHomeDir());

  if (dialog.ShowModal() == wxID_OK)
    {
      path=dialog.GetPath();
      directory=dialog.GetDirectory();
      fname=dialog.GetFilename();
      network->Save(path);
    }
}

void AppFrame::Open(wxCommandEvent& WXUNUSED(event))
{
  wxFileDialog dialog
    (
     this,
     _T("Open File dialog"),
     directory,
     fname,
     _T("Network files (*.nt)|*.nt")
     );
  
  if (directory=="")
    dialog.SetDirectory(wxGetHomeDir());
  
  if (dialog.ShowModal() == wxID_OK)
    {
      path=dialog.GetPath();
      directory=dialog.GetDirectory();
      fname=dialog.GetFilename();
      network->Load(path);
    }
}

void AppFrame::LoadFromServer( wxCommandEvent& WXUNUSED(event) )
{
   char* nw_str = 0;
   try
   {
      nw_str=network->exec->GetNetwork();
   }
   catch ( CORBA::Exception& )
   {
      Log("No VE_CE found!\n");
   }

   network->LoadS(nw_str);
   delete nw_str;
}

void AppFrame::New( wxCommandEvent& WXUNUSED(event) )
{
   network->New();
}

void AppFrame::SubmitToServer( wxCommandEvent& WXUNUSED(event) )
{
   std::string nw_str;
   network->SaveS(nw_str);
   try 
   {
      network->exec->SetNetwork(nw_str.c_str());
      run_menu->Enable(v21ID_START_CALC, true);
   }
   catch ( CORBA::Exception& ) 
   {
      Log("no exec found!\n");
   }
}

void AppFrame::StartCalc( wxCommandEvent& WXUNUSED(event) )
{
   try	
   { 
      network->exec->StartCalc();
      run_menu->Enable(v21ID_START_CALC, false);
   }
   catch ( CORBA::Exception& ) 
   {
      Log("no exec found!\n");
   }
}

void AppFrame::StopCalc(wxCommandEvent& WXUNUSED(event) )
{
   try
   {
      network->exec->StopCalc();
   }
   catch ( CORBA::Exception& )
   {
      Log("no exec found!\n");
   }
}

void AppFrame::PauseCalc(wxCommandEvent& WXUNUSED(event) )
{
   try
   {
      network->exec->PauseCalc();
   }
   catch ( CORBA::Exception& )
   {
      Log("no exec found!\n");
   }
}

void AppFrame::ResumeCalc(wxCommandEvent& WXUNUSED(event) )
{
   try
   { 
      network->exec->Resume();
   }
   catch ( CORBA::Exception& )
   {
      Log("no exec found!\n");
   }
}

void AppFrame::ViewResult(wxCommandEvent& WXUNUSED(event) )
{
   char* result;
   //char buf[80];
   std::map<int, MODULE>::iterator iter;
   unsigned int i;
   std::vector<wxString> titles;
   //TextResultDialog * result_dlg;
   SummaryResultDialog * result_dlg;
   std::vector<wxString> v_desc, v_value;
   std::vector<std::string> descs;
   std::vector<int> alignments;

   titles.push_back("Description");
   alignments.push_back (wxALIGN_LEFT);
   titles.push_back("Value");
   alignments.push_back (wxALIGN_RIGHT);

   /*
   result_dlg = new TextResultDialog(NULL);
   result_dlg->syngas->Clear();
   result_dlg->syngas->AddRow(titles);
   */
   result_dlg = new SummaryResultDialog(NULL, wxT("Result Summary"), wxSize(560, 400));
   result_dlg->syngas->Clear();
   result_dlg->syngas->SetNumofCols( 2 );
   result_dlg->syngas->SetColTitles( titles );
   result_dlg->syngas->SetColAlignments( alignments );

   if (!CORBA::is_nil( network->exec.in() )) 
   {
      try 
      {
         for (iter=network->modules.begin(); iter!=network->modules.end(); iter++) 
         {
	         result = network->exec->GetModuleResult(iter->first);
	
	         if ( std::string(result) != "" ) 
            {
               Package p;
               p.SetSysId("linkresult.xml");
               p.Load(result, strlen(result));

               descs = p.GetInterfaceVector()[0].getStrings();
               
               // This may not be needed.
               // this should be taken care of with previous statement
               if ( descs.size() < 1 ) 
                  continue;
               
               v_desc.clear();
               v_value.clear();
               /*
               v_desc.push_back(iter->second.pl_mod->GetName());
               std::ostringstream dirStringStream;
               dirStringStream << "   " << iter->first;
               std::string dirString = dirStringStream.str();
               v_value.push_back(dirString.c_str());
               */
               wxString str;
               str = iter->second.pl_mod->GetName();
               str << " (" << iter->first << ")";
               result_dlg->NewTab( str );

               for (i=0; i<descs.size(); i++) 
               {
	               std::string desc = descs[i];
	               std::string value = p.GetInterfaceVector()[0].getString(descs[i]);

                  if (desc.substr(0,3) == "***") 
                  {
                     desc = desc.substr(9,desc.size()-9);
                  }

                  v_desc.push_back( desc.c_str() );
                  v_value.push_back( value.c_str() );
               }

               /*result_dlg->syngas->AddSeperator(' ');
               result_dlg->syngas->AddSeperator('+');
               result_dlg->syngas->AddSeperator(' ');*/
               result_dlg->Set2Cols(v_desc, v_value);
	         }
         }
    
         result = network->exec->GetModuleResult(-1); //Global result
      
         if (std::string(result)!="") 
         {
            Package p;
            p.SetSysId("linkresult.xml");
            p.Load(result, strlen(result));

            descs = p.GetInterfaceVector()[0].getStrings();
            v_desc.clear();
            v_value.clear();
            //v_desc.push_back("Plant Results");
            //v_value.push_back("   ");
	         result_dlg->NewTab( wxT("Plant Results") );

	         for (i=0; i<descs.size(); i++) 
            {
	            v_desc.push_back(descs[i].c_str());
	            v_value.push_back((p.GetInterfaceVector()[0].getString(descs[i])).c_str());
	         }
	         /*result_dlg->syngas->AddSeperator(' ');
	         result_dlg->syngas->AddSeperator('+');
	         result_dlg->syngas->AddSeperator(' ');*/
	         result_dlg->Set2Cols(v_desc, v_value);
         }
      }
      catch (CORBA::Exception &) 
      {
         std::cerr << "Maybe Computational Engine is down " << std::endl;
         return;
      }
   }
   else 
   {
      titles.clear();
      titles.push_back("No Plant Results");
      titles.push_back(" ");
  
      result_dlg->syngas->AddSeperator('+');
      result_dlg->syngas->AddRow(titles);
   }
       
   // EPRI TAG
   std::vector<wxString> v_desc2, v_value2;
   double total_cccost = 0;
   double total_omcost = 0;
   v_desc.clear();
   v_value.clear();
   for (iter=network->modules.begin(); iter!=network->modules.end(); iter++) 
   {
      if(iter->second.pl_mod->financial_dlg != NULL) 
      {
         if(iter->second.pl_mod->financial_dlg->_use_data) 
         {
	         double TPC = iter->second.pl_mod->financial_dlg->_cc00_d *
	                        (1 +
	                     iter->second.pl_mod->financial_dlg->_cc01_d / 100 +
	                     iter->second.pl_mod->financial_dlg->_cc02_d / 100 +
	                     iter->second.pl_mod->financial_dlg->_cc03_d / 100 +
	                     iter->second.pl_mod->financial_dlg->_cc04_d / 100 +
	                     iter->second.pl_mod->financial_dlg->_cc05_d / 100);

	         double TPI = TPC + iter->second.pl_mod->financial_dlg->_cc06_d;

	         double cccost = TPI + iter->second.pl_mod->financial_dlg->_cc07_d +
	                           iter->second.pl_mod->financial_dlg->_cc08_d;

	         total_cccost += cccost;

	         v_desc.push_back(wxString(iter->second.pl_mod->GetName()));
	         //sprintf(buf,"%.2f", cccost);
            std::ostringstream dirStringStream;
            dirStringStream << std::setprecision(2) << cccost;
            std::string dirString = dirStringStream.str();

	         v_value.push_back(dirString.c_str());

	         double TMC = TPC * iter->second.pl_mod->financial_dlg->_om00_d / 100;
	
	         double omcost = TMC + 
	                        iter->second.pl_mod->financial_dlg->_om01_d +
	                        iter->second.pl_mod->financial_dlg->_om02_d +
	                        iter->second.pl_mod->financial_dlg->_om02_d * iter->second.pl_mod->financial_dlg->_om03_d / 100;

	         total_omcost += omcost;
	
	         v_desc2.push_back(wxString(iter->second.pl_mod->GetName()));
	         //sprintf(buf,"%.2f", omcost);
            dirStringStream << std::setprecision(2) << omcost;
	         v_value2.push_back(dirString.c_str());
         }
      }
   }

   if ( v_desc.size() > 0 ) 
   {
      result_dlg->syngas->AddSeperator(' ');
      result_dlg->syngas->AddSeperator(' ');

      titles.clear();
      titles.push_back("Plant Component");
      titles.push_back("Capital Required (M$)");

      result_dlg->syngas->AddRow(titles);
      result_dlg->syngas->AddSeperator('+');

      v_desc.push_back("Total");
      //sprintf(buf,"%.2f", total_cccost);
      std::ostringstream dirStringStream;
      dirStringStream << std::setprecision(2) << total_cccost;
      std::string dirString = dirStringStream.str();
      v_value.push_back(dirString.c_str());

      result_dlg->Set2Cols(v_desc, v_value);

      //

      result_dlg->syngas->AddSeperator(' ');

      titles.clear();
      titles.push_back("Plant Component");
      titles.push_back("Revenue Required (M$)");

      result_dlg->syngas->AddRow(titles);
      result_dlg->syngas->AddSeperator('+');

      v_desc2.push_back("Total");
      //sprintf(buf,"%.2f", total_omcost);
      dirStringStream << std::setprecision(2) << total_omcost;
      v_value2.push_back(dirString.c_str());

      result_dlg->Set2Cols(v_desc2, v_value2);
   }

   result_dlg->ShowModal();

   delete result_dlg;
}

void AppFrame::GlobalParam(wxCommandEvent& WXUNUSED(event) )
{
  if (network->globalparam_dlg!=NULL)
    network->globalparam_dlg->Show();
  else
    {
      network->globalparam_dlg=new GlobalParamDialog(this, -1);
      network->globalparam_dlg->Show();
    }
  
}

void AppFrame::ConExeServer( wxCommandEvent& WXUNUSED(event) )
{
   wxImage splashImage(ve_ce_banner_xpm);
   wxBitmap bitmap(splashImage);
   wxSplashScreen* splash = new wxSplashScreen(bitmap,
           wxSPLASH_CENTRE_ON_PARENT|wxSPLASH_TIMEOUT,
          2500, this, -1, wxDefaultPosition, wxDefaultSize,
          wxSIMPLE_BORDER|wxSTAY_ON_TOP);
   //wxSafeYield();
   if ( pelog==NULL )
   {
	   pelog = new PEThread(this);
	   pelog->activate();
   }

   if (!is_orb_init)
   {
      if (init_orb_naming())
	      is_orb_init = true;
      else
	      return;
   }
  
   try
   { 
      //_mutex.acquire();	  
      OrbThread* ot = new OrbThread(this);
      ot->activate();
      //ot->Run();
      //register it to the server
      //_mutex.acquire();
    
      //_mutex.release();
      //Enalbe Menu items
   } 
   catch ( CORBA::Exception& ) 
   {
      Log("Can't find executive or UI registration error\n");
   }
}
  
void AppFrame::ConVEServer(wxCommandEvent &WXUNUSED(event))
{
   if (pelog==NULL)
   {
	   pelog = new PEThread(this);
	   pelog->activate();
   }

   if (!is_orb_init)
   {
      if (init_orb_naming())
	      is_orb_init=true;
      else
	      return;
   }

   wxImage splashImage(ve_xplorer_banner_xpm);
   wxBitmap bitmap(splashImage);
   wxSplashScreen* splash = new wxSplashScreen(bitmap,
           wxSPLASH_CENTRE_ON_PARENT|wxSPLASH_TIMEOUT,
          2500, this, -1, wxDefaultPosition, wxDefaultSize,
          wxSIMPLE_BORDER|wxSTAY_ON_TOP);
   //wxSafeYield();
  
   try 
   {
      CosNaming::Name name(1);
      name.length(1);
      //Now get the reference of the VE server
      name[0].id   = CORBA::string_dup ("Master");
      name[0].kind = CORBA::string_dup ("VE_Xplorer");
      CORBA::Object_var naming_context_object =
      orb->resolve_initial_references ("NameService");
      CosNaming::NamingContext_var naming_context1 = CosNaming::NamingContext::_narrow (naming_context_object.in ());
      CORBA::Object_var ve_object = naming_context1->resolve(name);
      vjobs = VjObs::_narrow(ve_object.in());

      if (CORBA::is_nil(vjobs.in()))
         std::cerr<<"VjObs is Nill"<<std::endl;
    
      //Create the VE Tab
      con_menu->Enable(v21ID_CONNECT_VE, false);
      con_menu->Enable(v21ID_DISCONNECT_VE, true);
   } 
   catch (CORBA::Exception &) 
   {
      Log("Can't find VE server\n");
      return;
   }
  
   CreateVETab();
   Log("Connected to VE server.\n");
}

bool AppFrame::init_orb_naming()
{
   try 
   {
      // First initialize the ORB, 
      orb = CORBA::ORB_init (wxGetApp().argc, wxGetApp().argv,
                       ""); // the ORB name, it can be anything! 
    
      //Here is the part to contact the naming service and get the reference for the executive
      CORBA::Object_var naming_context_object =
         orb->resolve_initial_references ("NameService");
      naming_context = CosNaming::NamingContext::_narrow (naming_context_object.in ());
      /*if (naming_context==CORBA)
      {
         poa->destroy (1, 1);
		    // Finally destroy the ORB
		    orb->destroy();
		    std::cerr << "Naming service not found!" << std::endl;
		    return false;
		    }
      */
      Log("Initialized ORB and connection to the Naming Service\n");
      return true;
   }
   catch ( CORBA::Exception& ) 
   {  
      //		poa->destroy (1, 1);
      // Finally destroy the ORB
      orb->destroy();
      Log("CORBA exception raised! Can't init ORB or can't connect to the Naming Service\n");
      return false;
   }
}

void AppFrame::LoadBase(wxCommandEvent &WXUNUSED(event))
{
  network->Load("IECMBase.nt");
}

void AppFrame::LoadSour(wxCommandEvent &WXUNUSED(event))
{
  network->Load("IECMSour.nt");
}

void AppFrame::LoadREIBase(wxCommandEvent &WXUNUSED(event))
{
  network->Load("REIBase.nt");
}
void AppFrame::LoadREISour(wxCommandEvent &WXUNUSED(event))
{
  network->Load("REISour.nt");
}

void AppFrame::Log(const char* msg)
{
  if (pelog!=NULL)
    pelog->SetMessage(msg);
  
  //::wxPostEvent(this, u);
}

void AppFrame::OnUpdateUIPop(wxUpdateUIEvent& event )
{
  logwindow->AppendText(event.GetText());
}

void AppFrame::DisConExeServer(wxCommandEvent &WXUNUSED(event))
{
   try
   {
      network->exec->UnRegisterUI(p_ui_i->UIName_.c_str());
      delete p_ui_i;
      p_ui_i = NULL;

      con_menu->Enable(v21ID_SUBMIT,false);
      con_menu->Enable(v21ID_LOAD, false);
      con_menu->Enable(v21ID_CONNECT, true);
      run_menu->Enable(v21ID_START_CALC, false);
      // EPRI TAG run_menu->Enable(v21ID_VIEW_RESULT, false);
      con_menu->Enable(v21ID_DISCONNECT, false);
    
      Log("Disconnect successful.\n");
   }
   catch (CORBA::Exception &) 
   {
      Log("Disconnect failed.\n");
   }
}

void AppFrame::DisConVEServer(wxCommandEvent &WXUNUSED(event))
{
  //try {
  /*CosNaming::Name name(1);
    
    name.length(1);
    name[0].id   = (const char*) "Master";
    name[0].kind = (const char*) "VE_Xplorer";
    
    try
    {
    //vprDEBUG(vprDBG_ALL,0) 
    //<< "naming_context->unbind for CORBA Object  " 
    //<< std::endl << vprDEBUG_FLUSH;
    naming_context->unbind( name );
    //naming_context->destroy();
    }
    catch( CosNaming::NamingContext::InvalidName& )
    {
    std::cerr << "Invalid name for CORBA Object  " << std::endl;
    }
    catch(CosNaming::NamingContext::NotFound& ex)
    {
    std::cerr << "Name not found for CORBA Object  " << ex.why << std::endl;
    }*/
  wx_ve_splitter->Unsplit(m_frame);
  sizerTab->Detach(m_frame);
  delete m_frame;
  m_frame = NULL;
  con_menu->Enable(v21ID_CONNECT_VE, true);
  con_menu->Enable(v21ID_DISCONNECT_VE, false);
  Log("Disconnect VE suceeded.\n");
  //}catch (CORBA::Exception &) {
  
  //Log("Disconnect VE failed.\n");
  //}
  
  //delete vjobs;

}

void AppFrame::ViewHelp(wxCommandEvent& WXUNUSED(event))
{
   char browser[1024];
   wxString help;
   wxString fgroot;
   wxString docdir;
   wxString cmd;
   fgroot = getenv("FGROOT");
  
#ifdef WIN32
   docdir=fgroot+"\\Framework\\doc";
   help = fgroot+"\\Framework\\doc\\index.html";
   FindExecutable("index.html", docdir.c_str(), browser);
#endif
   cmd="\"";
   cmd+=browser;
   cmd+="\" \"";
   cmd+=help;
   cmd+="\"";
  
   ::wxExecute(cmd, wxEXEC_ASYNC|wxEXEC_MAKE_GROUP_LEADER);
}

void AppFrame::CloseVE()
{

}
