#include "Frame.h"
#include "wx/imaglist.h"
#include "wx/artprov.h"
#include "ResultPanel.h"
#include "App.h"
#include "package.h"
#include "OrbThread.h"

BEGIN_EVENT_TABLE (AppFrame, wxFrame)
  EVT_CLOSE(AppFrame::OnClose)
  EVT_MENU(v21ID_ZOOMIN, AppFrame::ZoomIn)
  EVT_MENU(v21ID_ZOOMOUT, AppFrame::ZoomOut)
  EVT_MENU(wxID_SAVE, AppFrame::Save)
  EVT_MENU(wxID_SAVEAS, AppFrame::SaveAs)
  EVT_MENU(wxID_NEW, AppFrame::New)
  EVT_MENU(wxID_EXIT, wxFrame::Close)
  EVT_MENU(wxID_OPEN, AppFrame::Open)
  EVT_MENU(v21ID_LOAD, AppFrame::LoadFromServer)
  EVT_MENU(v21ID_SUBMIT, AppFrame::SubmitToServer)
  EVT_MENU(v21ID_CONNECT, AppFrame::ConExeServer)
  EVT_MENU(v21ID_DISCONNECT, AppFrame::DisConExeServer)
  EVT_MENU(v21ID_CONNECT_VE, AppFrame::ConVEServer)
  EVT_MENU(v21ID_START_CALC, AppFrame::StartCalc)
  EVT_MENU(v21ID_STOP_CALC, AppFrame::StopCalc)
  EVT_MENU(v21ID_PAUSE_CALC, AppFrame::PauseCalc)
  EVT_MENU(v21ID_RESUME_CALC, AppFrame::ResumeCalc)
  EVT_MENU(v21ID_HELP, AppFrame::ViewHelp)
//  EVT_MENU(v21ID_VIEW_RESULT, AppFrame::ViewResult)
//  EVT_MENU(v21ID_GLOBAL_PARAM, AppFrame::GlobalParam)
//  EVT_MENU(v21ID_BASE, AppFrame::LoadBase)
//  EVT_MENU(v21ID_SOUR, AppFrame::LoadSour)
//  EVT_MENU(v21ID_REI_BASE, AppFrame::LoadREIBase)
//  EVT_MENU(v21ID_REI_SOUR, AppFrame::LoadREISour)
  EVT_UPDATE_UI(7777, AppFrame::OnUpdateUIPop)
END_EVENT_TABLE()

AppFrame::AppFrame(wxWindow * parent, wxWindowID id, const wxString& title)
  :wxFrame(parent, id, title)
{
  wx_log_splitter = new wxSplitterWindow(this, -1);
  wx_ve_splitter = new wxSplitterWindow(wx_log_splitter, -1);
  wx_nw_splitter = new wxSplitterWindow(wx_ve_splitter, -1);
  
  //LogWindow
  logwindow = new wxTextCtrl(wx_log_splitter, MYLOG, "", wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxTE_READONLY);

  // VE Tabs
  //m_tabs = ( UI_Tabs*) NULL;
  m_frame = ( UI_Frame*) NULL;
  is_orb_init= false;
  p_ui_i=NULL;	
  av_modules = new Avail_Modules(wx_nw_splitter, TREE_CTRL, wxDefaultPosition, wxDefaultSize, wxTR_HAS_BUTTONS);
  network = new Network(wx_nw_splitter, -1 );
  av_modules->SetNetwork(network);
	
  wx_log_splitter->SplitHorizontally(wx_ve_splitter, logwindow, -200);
  wx_nw_splitter->SplitVertically(av_modules, network, 200);  
  wx_ve_splitter->Initialize(wx_nw_splitter);
  SetSize(DetermineFrameSize(NULL));
  CreateMenu();
  CreateStatusBar();
  SetStatusText("New Vison 21");
  
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
   m_frame = new UI_Frame(vjobs.in(), wx_ve_splitter, ID_UI_TABS);
   // Create the notebook's panels
   //m_tabs->AssignImageList(m_imageList);
   //m_frame->_tabs->AssignImageList(m_imageList);
   //m_tabs->createTabPages();
   //wxNotebookSizer *nbs = new wxNotebookSizer(m_tabs);
   wxBoxSizer *sizerTab = new wxBoxSizer(wxVERTICAL);
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

wxRect AppFrame::DetermineFrameSize (wxConfig* config) {
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

void AppFrame::StoreFrameSize (wxRect rect, wxConfig* config) {

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

void AppFrame::OnClose(wxCloseEvent &event)
{
  if (is_orb_init)
  {
	CosNaming::Name UIname(1);
	UIname.length(1);
	if (p_ui_i!=NULL && p_ui_i->UIName_!="")
	  {
	    UIname[0].id = CORBA::string_dup ((p_ui_i->UIName_).c_str());
	    naming_context->unbind(UIname);
	  
	    poa->destroy (1, 1);
	  }
    orb->destroy();
  }

  StoreFrameSize(GetRect(), NULL);
  Destroy();
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

  con_menu->Enable(v21ID_SUBMIT,false);
  con_menu->Enable(v21ID_LOAD, false);
  con_menu->Enable(v21ID_DISCONNECT, false);

  
  run_menu->Append(v21ID_START_CALC, _("Start Simulation"));
  run_menu->Append(v21ID_STOP_CALC, _("Stop Simulation"));
  run_menu->Append(v21ID_PAUSE_CALC, _("Pause Simulation"));
  run_menu->Append(v21ID_RESUME_CALC, _("Resume Simulation"));
//  run_menu->Append(v21ID_VIEW_RESULT, _("View Results"));
//  run_menu->Append(v21ID_GLOBAL_PARAM, _("Global Parameters"));
  //  run_menu->Append(v21ID_VIEW_FINANCIAL, _("View Financial Params"));

  run_menu->Enable(v21ID_START_CALC, false);
  run_menu->Enable(v21ID_STOP_CALC, false);
  run_menu->Enable(v21ID_PAUSE_CALC, false);
  run_menu->Enable(v21ID_RESUME_CALC, false);
 // run_menu->Enable(v21ID_VIEW_RESULT, false);
  
  
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

void AppFrame::ZoomIn(wxCommandEvent &event)
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

void AppFrame::ZoomOut(wxCommandEvent &event)
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

void AppFrame::Save(wxCommandEvent &event)
{
  if (path==wxString("")) //First time call save will be the same as SaveAs
    SaveAs(event);
  else
    network->Save(path);
}

void AppFrame::SaveAs(wxCommandEvent &event)
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

void AppFrame::Open(wxCommandEvent &event)
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

void AppFrame::LoadFromServer(wxCommandEvent &event)
{
	char *nw_str;
	try	{ 
		nw_str=network->exec->GetNetwork();
    
	}	catch (CORBA::Exception &) {
		std::cout << "no exec found!" << std::endl;
	}

	network->LoadS(nw_str);
	delete nw_str;
}

void AppFrame::New(wxCommandEvent &event)
{
  network->New();
}
void AppFrame::SubmitToServer(wxCommandEvent &event)
{
  std::string nw_str;
  network->SaveS(nw_str);
  try {
    network->exec->SetNetwork(nw_str.c_str());
  }catch (CORBA::Exception &) {
    std::cout << "no exec found!" << std::endl;
  }
  
  
}

void AppFrame::StartCalc(wxCommandEvent &event)
{
	try	{ 
		network->exec->StartCalc();
    
	}	catch (CORBA::Exception &) {
		std::cout << "no exec found!" << std::endl;
	}

}

void AppFrame::StopCalc(wxCommandEvent &event)
{
	try	{ 
		network->exec->StopCalc();
    
	}	catch (CORBA::Exception &) {
		std::cout << "no exec found!" << std::endl;
	}

}

void AppFrame::PauseCalc(wxCommandEvent &event)
{
	try	{ 
		network->exec->PauseCalc();
    
	}	catch (CORBA::Exception &) {
		std::cout << "no exec found!" << std::endl;
	}
}

void AppFrame::ResumeCalc(wxCommandEvent &event)
{
	try	{ 
		network->exec->Resume();
    
	}	catch (CORBA::Exception &) {
		std::cout << "no exec found!" << std::endl;
	}

}

void AppFrame::ViewResult(wxCommandEvent &event)
{
  unsigned int i;
  long mod, port;
  int pos;
  wxString result;
  Interface resultintf;
  ResultPanel_Dialog dlg(NULL, -1);
  long gasi_mod_id=-1, selx_mod_id=-1;
  map<int, MODULE>::iterator iter;
  
  for (iter=network->modules.begin(); iter!=network->modules.end(); iter++)
    {
      i=iter->first;
      if (pos=network->modules[i].cls_name.find("GASI")!=string::npos)
	gasi_mod_id = network->modules[i].pl_mod->GetID();
      if (pos=network->modules[i].cls_name.find("REI_Gasi")!=string::npos)
	gasi_mod_id = network->modules[i].pl_mod->GetID();
      if (pos=network->modules[i].cls_name.find("SELX")!=string::npos)
	selx_mod_id = network->modules[i].pl_mod->GetID();
    }
  try {
    port = 0;

    //Get the overall network data
    mod = -1;
    result=network->exec->GetExportData(mod, port);
    //network->ReadForInterfaceS(result, resultintf);
    Package p;

    p.SetSysId("temp");
    p.Load((const char*)result, strlen(result));
    resultintf = p.intfs[0];

    dlg.mw_gross_ = resultintf.getDouble("MW_GROSS");
    dlg.mw_net_ = resultintf.getDouble("MW_NET");
    dlg.net_eff_ = resultintf.getDouble("NET_EFF");
    dlg.capital_cst_ = resultintf.getDouble("CAPITAL_CST");
	dlg.elec_cst_ = resultintf.getDouble("ELEC_CST");

    //Get the gasifier stuff
    if(gasi_mod_id>0) {
	  mod = gasi_mod_id;
      result=network->exec->GetExportData(mod, port);
      p.Load(result, strlen(result));
      resultintf = p.intfs[0];
      // network->ReadForInterfaceS(result, resultintf);
      dlg.coal_in_ = resultintf.getDouble("COAL_IN");
      dlg.water_in_ = resultintf.getDouble("WATER_IN");
      dlg.oxid_in_ = resultintf.getDouble("OX_IN");
	} else {
      dlg.coal_in_ = 0;
      dlg.water_in_ = 0;
      dlg.oxid_in_ = 0;
	}

    //Get the selx stuff
    if(selx_mod_id>0) {
      mod = selx_mod_id;
      result=network->exec->GetExportData(mod, port);
      p.Load(result, strlen(result));
      resultintf = p.intfs[0];
      //network->ReadForInterfaceS(result, resultintf);
      dlg.co2_in_ = resultintf.getDouble("CO2_IN");
      dlg.co2_out_ = resultintf.getDouble("CO2_OUT");
      dlg.co2_cap_ = resultintf.getDouble("CO2_CAP");
	} else {
      dlg.co2_in_ = 0;
      dlg.co2_out_ = 0;
      dlg.co2_cap_ = 0;
	}

  } catch(CORBA::Exception &) {
    cerr << "ViewResult ????\n";
  }

  dlg.ShowModal();
}

void AppFrame::GlobalParam(wxCommandEvent &event)
{
  if (network->globalparam_dlg!=NULL)
    network->globalparam_dlg->Show();
  //  else
  //  {
  //    network->globalparam_dlg=new GlobalParamDialog(this, -1);
  //    network->globalparam_dlg->Show();
  //  }
  
}

void AppFrame::ConExeServer(wxCommandEvent &event)
{
  if (!is_orb_init)
  {
    if (init_orb_naming())
      is_orb_init = true;
    else
      return;
  }

  try{

		//_mutex.acquire();	  
    	  if (pelog==NULL)
		  {
		    pelog = new PEThread(this);
		    pelog->activate();
		  }

		OrbThread* ot = new OrbThread(this);
		ot->activate();

		//ot->Run();
		//register it to the server
		//_mutex.acquire();

		//_mutex.release();
		//Enalbe Menu items

  

	} catch (CORBA::Exception &) {
		
		Log("Can't find executive or UI registration error\n");
	}
}
  
void AppFrame::ConVEServer(wxCommandEvent &event)
{
	if (!is_orb_init)
	{
		if (init_orb_naming())
		  is_orb_init=true;
		else
		  return;
	}

	try {

    	  if (pelog==NULL)
		  {
		    pelog = new PEThread(this);
		    pelog->activate();
		  }

	    CosNaming::Name name(1);
		name.length(1);
		//Now get the reference of the VE server
		name[0].id   = (const char*) "Master";
		name[0].kind = (const char*) "VE_Xplorer";
		CORBA::Object_var ve_object = naming_context->resolve(name);
		vjobs = VjObs::_narrow(ve_object.in());
		if (CORBA::is_nil(vjobs.in()))
			std::cerr<<"VjObs is Nill"<<std::endl;
		
		//Create the VE Tab
		CreateVETab();
		con_menu->Enable(v21ID_CONNECT_VE, false);
		Log("Found VE server\n");
	} catch (CORBA::Exception &) {
		
		Log("Can't find VE server\n");
	}
}

bool AppFrame::init_orb_naming()
{
//	char *argv[]={""};
//	int argc = 0;
cout << " orb init " << endl;
	try {
		// First initialize the ORB, 
		orb =
			CORBA::ORB_init (wxGetApp().argc, wxGetApp().argv,
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
		    cerr << "Naming service not found!" << endl;
		    return false;
		  }
		  */  
		return true;
	}  catch (CORBA::Exception &) {
	//		poa->destroy (1, 1);
		// Finally destroy the ORB
		orb->destroy();
		Log("CORBA exception raised! Can't init ORB or can't connect to the Naming Service\n");
		return false;
	}
}

void AppFrame::LoadBase(wxCommandEvent &event)
{
  network->Load("IECMBase.nt");
}

void AppFrame::LoadSour(wxCommandEvent &event)
{
  network->Load("IECMSour.nt");
}

void AppFrame::LoadREIBase(wxCommandEvent &event)
{
  network->Load("REIBase.nt");
}
void AppFrame::LoadREISour(wxCommandEvent &event)
{
  network->Load("REISour.nt");
}

void AppFrame::Log(const char* msg)
{
	
  if (pelog!=NULL)
    pelog->SetMessage(msg);
    
	//::wxPostEvent(this, u);
}

void AppFrame::OnUpdateUIPop(wxUpdateUIEvent& event)
{
	logwindow->AppendText(event.GetText());
}

void AppFrame::DisConExeServer(wxCommandEvent &event)
{
	try {
			network->exec->UnRegisterUI(p_ui_i->UIName_.c_str());
			con_menu->Enable(v21ID_SUBMIT,false);
			con_menu->Enable(v21ID_LOAD, false);
			con_menu->Enable(v21ID_CONNECT, true);
			run_menu->Enable(v21ID_START_CALC, false);
//			run_menu->Enable(v21ID_VIEW_RESULT, false);
			con_menu->Enable(v21ID_DISCONNECT, false);
			Log("Disconnect suceeded.\n");
		}catch (CORBA::Exception &) {
		
			Log("Disconnect failed.\n");
		}
	

}

void AppFrame::ViewHelp(wxCommandEvent& event)
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
