#include "vtkDataSet.h"
#include "readWriteVtkThings.h"
//#include "makeSurf.h"
#ifndef _MAKEFRAME_H
#define _MAKEFRAME_H


class makeFrame : public wxFrame
{
	private:
		wxMenuBar *mBar;
		wxMenu *fileMenu;
		wxMenu *aboutMenu;
		wxPanel *panel;
		
		wxStaticBox* stBox;
		
		wxArrayString* filNamesList;
		
		wxTextCtrl* txt;
		wxButton* okButton;
		wxButton* cancelButton;
		wxRadioButton* extValRadioButton;
		wxRadioButton* isoValRadioButton;
		wxStaticBox* stBox_Selection;
		wxSlider* sliderSelection;
      wxSlider* sliderISO;
		wxStaticBox* stTxtBox;
      wxStaticBox* stISOBox;
		wxDialog* dlg;
		float valu;	//value from the slider
		enum{ MENU_OPEN, MENU_CONV_ASCII, MENU_CONV_BINARY, MENU_MAK_VTK_SURF,
				MENU_DEL, MENU_QT, MENU_ABOUT };
		enum{ ID_PANEL, ID_TXT_CTRL, ID_STATIC_BOX,ID_STATIC_BOX_SELECTION  };
		enum{ BTN_OPEN_ID, BTN_ID_ASCII, BTN_ID_BINARY, BTN_ID_SURF, DEL_ICON };
		enum{ EXT_VAL_RADIO_BTN, ISO_VAL_RADIO_BTN, SLIDER_SELECTION, SLIDER_ISO };
		
		//----------VTK stuff
		vtkDataSet* dataset;

	protected:
		DECLARE_EVENT_TABLE()
	public:
		makeFrame( const wxString title, int x, int y, int w, int h );
		~makeFrame( );
		bool InitToolbar(wxToolBar* toolBar);
		void onFileOpen( wxCommandEvent &event );
		void onFileConvAscii( wxCommandEvent &event );
		void onFileConvBinary( wxCommandEvent &event );
		void onFileMakVtkSurf( wxCommandEvent &event );
		void onDel( wxCommandEvent &event );
		void onFileQuit( wxCommandEvent &event );
		void onAbout( wxCommandEvent &event );
      void onExtValRadioButton( wxCommandEvent &event );
};

makeFrame::makeFrame(const wxString title, int x, int y, int w, int h)
       : wxFrame((wxFrame *) NULL, -1, title, wxPoint(x, y), wxSize(w, h))
		
{
	panel = new wxPanel( this, ID_PANEL, wxDefaultPosition,
			wxDefaultSize,wxTAB_TRAVERSAL, wxString( " panel " ) );
	
	//------------CREATE A STATIC BOX
	
	
	stBox = new wxStaticBox( panel, ID_STATIC_BOX, wxString("Log Window"),			
			wxPoint(0,2), wxSize(400,200), 0, wxString("Static Box") );
	
	//------------CREATE A LOG WINDOW WITHIN THE STATIC BOX
	txt = new wxTextCtrl( panel, ID_TXT_CTRL,_T(""),wxPoint(03,18),
			wxSize(393,180), wxTE_MULTILINE, wxDefaultValidator,
			wxString("Text Control")  );
	txt->SetEditable( false );

	//-----------------SET MENUS ON THE MENU BAR-----------------
	mBar = new wxMenuBar( );
	fileMenu = new wxMenu( );
	//---------add items to File Menu
	fileMenu->Append( MENU_OPEN, "&Open...", "Opens a VTK file" );
	fileMenu->AppendSeparator();
	fileMenu->Append( MENU_CONV_ASCII, "&Convert to ASCII...", "Converts the file to ASCII format" );
	fileMenu->Append( MENU_CONV_BINARY, "&Convert to Binary...", "Converts the file to binary format" );
	fileMenu->Append( MENU_MAK_VTK_SURF, "&Make VTK Surface...", "To make a VTK surface" );
	fileMenu->AppendSeparator();
	fileMenu->Append( MENU_DEL, "&Delete Loaded Files", "Deletes files loaded into memory" );
	fileMenu->AppendSeparator();
	fileMenu->Append( MENU_QT, "&Quit" );
   
	aboutMenu = new wxMenu( );	
	aboutMenu->Append( MENU_ABOUT, "&About", "Information" );
	mBar->Append( fileMenu, "&File" );	      //add fileMenu to the menu bar
	mBar->	Append( aboutMenu, "&About" );	//add about menu to menu bar
	SetMenuBar( mBar );			               //add menu bar to frame
	filNamesList = new wxArrayString();	      //creat array list to store filenames
	dataset = NULL;
}

makeFrame::~makeFrame()
{
	delete panel;
}

bool makeFrame::InitToolbar( wxToolBar* toolBar )
{
	
	// Set up toolbar
  	wxBitmap* toolBarBitmaps[5];
  	toolBarBitmaps[0] = new wxBitmap( "open_new.xpm" );
	toolBarBitmaps[1] = new wxBitmap( "ascii_small_new.xpm" );
	
	toolBarBitmaps[2] = new wxBitmap( "binary.xpm" );
	toolBarBitmaps[3] = new wxBitmap( "surf_new_2.xpm" );
	toolBarBitmaps[4] = new wxBitmap( "delete-icon.xpm" );
  	int currentX = -1;
  	toolBar->AddTool(BTN_OPEN_ID, *(toolBarBitmaps[0]), wxNullBitmap, FALSE, 
			currentX, -1, (wxObject *) NULL, "Open");
	toolBar->AddSeparator();
		toolBar->AddTool(BTN_ID_ASCII, *(toolBarBitmaps[1]), wxNullBitmap, FALSE, 
			currentX, -1, (wxObject *) NULL,"Convert to ASCII");
	toolBar->AddSeparator();
	toolBar->AddTool(BTN_ID_BINARY, *(toolBarBitmaps[2]), wxNullBitmap, FALSE, 
			currentX, -1, (wxObject *) NULL,"Convert to Binary");
	toolBar->AddSeparator();
	toolBar->AddTool( BTN_ID_SURF, *(toolBarBitmaps[3]), wxNullBitmap, FALSE, 
			currentX, -1, (wxObject *) NULL,"Make VTK Surface");
  	toolBar->AddSeparator();
	toolBar->AddTool( DEL_ICON, *(toolBarBitmaps[4]), wxNullBitmap, FALSE, 
			currentX, -1, (wxObject *) NULL,"Remove loaded files");
	toolBar->AddSeparator();
	toolBar->Realize();
  	for ( int i = 0; i < 5; i++ )
    	delete toolBarBitmaps[i];
  	return true;
}

void makeFrame::onFileOpen( wxCommandEvent &event )
{
	wxFileDialog* filDialog = new wxFileDialog( this, wxString("Choose a VTK file"),
		wxString(""), wxString(""), wxString
		("VTK files (*.vtk)|*.vtk|All files (*.*)|*.*"), wxOPEN|wxMULTIPLE, 
			wxDefaultPosition );
	if ( filDialog->ShowModal() == wxID_OK )
	{
		wxArrayString* filOpenNames = new wxArrayString();//temp array for filenames
		filDialog->GetFilenames( *filOpenNames );
		txt->WriteText( wxString("Loaded... \n") );
		for (int i=0;i<(int)(filOpenNames->GetCount()); i++ )
		{
			txt->WriteText( filOpenNames->Item( i ) );
			txt->WriteText( "\n" );
		}
		for (int i=0;i<(int)(filOpenNames->GetCount()); i++ )
		{
			filNamesList->Add( filOpenNames->Item( i ), 1 );
		}
		delete filOpenNames;
	}
	filDialog->Destroy();
}

void makeFrame::onFileConvAscii( wxCommandEvent &event )
{	
	if ( ( (int)(filNamesList->GetCount() ) != 0 ) )
	{
      const char* ascii = "ascii_";
      wxString* tempString = new wxString();
		txt->WriteText( wxString("Converting to ASCII... \n") );
		for (int i=0;i<(int)(filNamesList->GetCount()); i++ )
		{
			*tempString = filNamesList->Item( i );
         txt->WriteText( *tempString );
         txt->WriteText( wxString(".........") );
			dataset = readVtkThing( (char*)( tempString->c_str() ), 0 );
  			tempString->Prepend( ascii );
			txt->WriteText( *tempString );
         txt->WriteText( wxString(".........OK") );
			txt->WriteText( "\n" );
         writeVtkThing( dataset, (char*)( tempString->c_str() ) );
         dataset->Delete();
		}
		delete tempString;
	}
	else txt->WriteText( wxString( "Select files to convert \n" ) );
}




void makeFrame::onFileConvBinary( wxCommandEvent &event )
{	
	if ( ( (int)(filNamesList->GetCount() ) != 0 ) )
	{
		const char* bin = "binary_";
		wxString* tempString = new wxString();
		txt->WriteText( wxString("Converting to Binary... \n") );
		for (int i=0;i<(int)(filNamesList->GetCount()); i++ )
		{
			*tempString = filNamesList->Item( i );
         txt->WriteText( *tempString );
         txt->WriteText( wxString(".........") );
			dataset = readVtkThing( (char*)( tempString->c_str() ), 0 );
  			tempString->Prepend( bin );
			txt->WriteText( *tempString );
         txt->WriteText( wxString(".........OK") );
			txt->WriteText( "\n" );
         writeVtkThing( dataset, (char*)( tempString->c_str() ), 1  );
         dataset->Delete();
		}
		delete tempString;
	}
	else txt->WriteText( wxString( "Select files to convert \n" ) );
}




void makeFrame::onFileMakVtkSurf( wxCommandEvent &event )
{
	if ( filNamesList->GetCount() !=0 )
	{
		txt->WriteText( wxString("Make VTK Surfaces \n") );
		dlg = new wxDialog( this, -1, wxString("Make VTK Surface"), 
			wxDefaultPosition,wxSize(400,220), wxDEFAULT_DIALOG_STYLE, wxString("dlg box") );

		stBox_Selection = new wxStaticBox( dlg, ID_STATIC_BOX_SELECTION, 
			wxString("Select Exterior/ISO Value"), wxPoint(2,2), wxSize(395,215), 0, 
			wxString("Static Box Selection") );
	
		okButton = new wxButton( dlg, wxID_OK, wxString("Ok"), wxPoint(110,170),
			wxSize(50,27), wxBU_EXACTFIT, wxDefaultValidator, wxString("ok Button") );
		cancelButton = new wxButton( dlg, wxID_CANCEL, wxString("Cancel"), wxPoint(220,170),
			wxSize(50,27), wxBU_EXACTFIT, wxDefaultValidator, wxString("ok Button") );
	
		extValRadioButton = new wxRadioButton( dlg, EXT_VAL_RADIO_BTN, wxString("Exterior Value"), 
		wxPoint(10,20),	wxDefaultSize, wxRB_SINGLE, wxDefaultValidator, wxString("ext val") );
		isoValRadioButton = new wxRadioButton( dlg, ISO_VAL_RADIO_BTN, wxString("ISO Value"), 
			wxPoint(10,80),	wxDefaultSize, wxRB_SINGLE, wxDefaultValidator, wxString("iso val") );
      
      stTxtBox = new wxStaticBox( dlg, -1, wxString("Decimation Value"), wxPoint(225,20), wxSize(165,60), 0, wxString("") );
            
		sliderSelection = new wxSlider( dlg, SLIDER_SELECTION, 5, 0, 10, wxPoint(240,35), 
			wxSize(140, 15), wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP,             
			wxDefaultValidator, wxString("Slider") );
      
      sliderISO = new wxSlider( dlg, SLIDER_ISO, 5, 0, 10, wxPoint(80,115), 
			wxSize(140, 10), wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP, wxDefaultValidator, wxString("Slider 2") );
      
      stISOBox = new wxStaticBox( dlg, -1, wxString("Scalar Range"), wxPoint(74,100), wxSize(155,60), 0, wxString("") );
      /*if ( extValRadioButton->GetValue() )
      {
         sliderISO->Disable();
         stISOBox->Disable();
      }
      else if ( isoValRadioButton->GetValue() )
      {
         
         sliderISO->Disable();
         stISOBox->Disable();
      }*/
		dlg->Center( wxBOTH );
      
		if ( dlg->ShowModal(  ) == wxID_OK )
		{
			if ( extValRadioButton->GetValue() )
			{
            wxFileDialog* saveDialog = new wxFileDialog( dlg, wxString("Save As"),
		         wxString(""), wxString(""), wxString("VTK file (*.vtk)|*.vtk|STL file (*.stl)|*.stl"),                   
                  wxSAVE|wxOVERWRITE_PROMPT, wxDefaultPosition );
            
            saveDialog->ShowModal();
				txt->WriteText( wxString( "EXT_VAL_RADIO_BTN " ) );
			}
         
			else if ( isoValRadioButton->GetValue() )
			{      
				txt->WriteText( wxString( "ISO_VAL_RADIO_BTN " ) );			
			}
			//txt->WriteText( wxString( "OK pressed... \n" ) );
			valu = sliderSelection->GetValue();
			valu = (float)(valu)/(float)(10);         
			*txt<<valu<<"\n";
         
		}
      
		else txt->WriteText( wxString( "Cancel pressed... \n" ) );
      
	}	
	else	txt->WriteText( wxString( "Select files to make surface \n" ) );
}

void makeFrame::onDel( wxCommandEvent &event )
{
	if ( filNamesList->GetCount() !=0 )
      
	{
		filNamesList->Clear();		
		txt->WriteText( wxString( "Files removed from list \n" ) );
	}
	else txt->WriteText( wxString( "No files to be deleted \n" ) );
}


void makeFrame::onFileQuit( wxCommandEvent &event )
{	
   Destroy();
}


void makeFrame::onAbout( wxCommandEvent &event )
{
   
	(void)wxMessageBox(_T("Convert from VTK format to ASCII or Binary formats"),
			
			_T("About Convert"));	
}



void makeFrame::onExtValRadioButton( wxCommandEvent &event )
{
   sliderISO->Disable();
}


#endif
