// For compilers that supports precompilation , includes "wx/wx.h"
#include "wx/wx.h"
#include "toolBarApp.h"
#include "makeFrame.h"
//#include "wx/log.h"

IMPLEMENT_APP(toolBarApp)

bool toolBarApp::OnInit()
{
       
	makeFrame *frame = new makeFrame( "Convert", 100, 100, 400, 300 );
	
	frame->CreateToolBar( wxNO_BORDER | wxTB_HORIZONTAL, -1, 
			wxString("toolBar") );
	frame->GetToolBar()->SetMargins( 2, 2 );
	//--------GET TOOLBAR ADDRESS IN THE FRAME
	wxToolBar* toolBar = frame->GetToolBar();
	//--------ADD TOOLBAR TO THE FRAME
	frame->InitToolbar( toolBar );
	//--------CREATE STATUSBAR
	frame->CreateStatusBar( true );
	frame->Show( true );
	SetTopWindow(frame);
       return true;
}

BEGIN_EVENT_TABLE( makeFrame, wxFrame )	
	EVT_MENU( MENU_OPEN, makeFrame::onFileOpen )
	EVT_MENU( MENU_CONV_ASCII, makeFrame::onFileConvAscii )
	EVT_MENU( MENU_CONV_BINARY, makeFrame::onFileConvBinary )		
	EVT_MENU( MENU_MAK_VTK_SURF, makeFrame::onFileMakVtkSurf )
	EVT_MENU( MENU_DEL, makeFrame::onDel )	
	EVT_MENU( MENU_QT, makeFrame::onFileQuit )
	EVT_MENU( MENU_ABOUT, makeFrame::onAbout )
END_EVENT_TABLE()

BEGIN_EVENT_TABLE( makePopupDialog, wxDialog )
   EVT_RADIOBOX( RADIO_BOX, makePopupDialog::onRadioBox )
END_EVENT_TABLE()
