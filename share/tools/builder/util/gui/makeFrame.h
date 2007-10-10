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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "vtkDataSet.h"
#include "readWriteVtkThings.h"
#include "makePopupDialog.h"
#include "cfdGrid2Surface.h"
#include "setScalarAndVector.h"
#include <vtkPolyData.h>
#include <vtkTriangleFilter.h>
#include <vtkSTLWriter.h>
#include <vtkGeometryFilter.h>
#include <vtkFloatArray.h>
#include <vtkPolyDataNormals.h>
#include <vtkContourFilter.h>

#include <vtkPointData.h>

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
      makePopupDialog* popup;
      float valu;	      //value from the slider
      double range[2];  //holds the value of the scalar range of data
		enum{ MENU_OPEN, MENU_CONV_ASCII, MENU_CONV_BINARY, MENU_MAK_VTK_SURF,
				MENU_DEL, MENU_QT, MENU_ABOUT };
		enum{ ID_PANEL, ID_TXT_CTRL, ID_STATIC_BOX,ID_STATIC_BOX_SELECTION  };
		enum{ BTN_OPEN_ID, BTN_ID_ASCII, BTN_ID_BINARY, BTN_ID_SURF, DEL_ICON };
		
		//----------VTK stuff
		vtkDataSet* dataset;
      vtkPolyData* surface;
      void writeVtkGeomToStl( vtkDataSet * vtkdataset, char filename [] );
      void writeIsoSurface( vtkDataSet * vtkdataset );
      
	protected:
		DECLARE_EVENT_TABLE()
      void onFileOpen( wxCommandEvent &event );
		void onFileConvAscii( wxCommandEvent &event );
		void onFileConvBinary( wxCommandEvent &event );
		void onFileMakVtkSurf( wxCommandEvent &event );
		void onDel( wxCommandEvent &event );
		void onFileQuit( wxCommandEvent &event );      
		void onAbout( wxCommandEvent &event );
      
	public:
		makeFrame( const wxString title, int x, int y, int w, int h );
		~makeFrame( );
		bool InitToolbar(wxToolBar* toolBar);
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
   surface = NULL;
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
	else txt->WriteText( wxString( "Select files to convert to ASCII\n" ) );
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
	else txt->WriteText( wxString( "Select files to convert to binary \n" ) );
}




void makeFrame::onFileMakVtkSurf( wxCommandEvent &event )
{		
	if ( filNamesList->GetCount() !=0 )
	{
      wxString* tempString = new wxString();
      for (int i=0;i<(int)(filNamesList->GetCount()); i++ )
		{
         *tempString = filNamesList->Item( i );
         dataset = readVtkThing( (char*)( tempString->c_str() ), 0 );
         dataset->GetScalarRange( range );
		   txt->WriteText( wxString("Make VTK Surfaces \n") );
         *txt<<range[0]<<" : "<<range[1]<<"\n";
         
         popup = new makePopupDialog( this, 400, 200, 
               range[0], range[1], *tempString );
      
         popup->Center( wxBOTH );

      
		   if ( popup->ShowModal(  ) == wxID_OK ) 
		   {
			   if ( popup->getRadioVal() )   //if Exterior valu radio button is selected
			   {
               
               wxFileDialog* saveDialog = new wxFileDialog( popup, wxString("Save As"),
		            wxString(""), wxString(""), wxString("VTK file (*.vtk)|*.vtk|STL file (*.stl)|*.stl"),                   
                     wxSAVE|wxOVERWRITE_PROMPT, wxDefaultPosition );
            
               if ( saveDialog->ShowModal() == wxID_OK )
               {
                  surface = cfdGrid2Surface( dataset, popup->getDecimationValu() );
                  if ( saveDialog->GetFilterIndex() == 0 )        //filetype VTK
                  {
                     writeVtkThing( surface, (char*)(saveDialog->GetFilename()).c_str(), 1 );
                  }
                  else if ( saveDialog->GetFilterIndex() == 1 )    //filetype STL
                  {                  
                     writeVtkGeomToStl( surface, (char*)(saveDialog->GetFilename()).c_str() );
                  }
                  txt->WriteText( "Saved as " + saveDialog->GetFilename() + "\n" );
                  surface->Delete();
               }

			   }
         
			   else if ( !popup->getRadioVal() )        //if Iso valu radio button is selected
			   {               
               wxFileDialog* saveDialog = new wxFileDialog( popup, wxString("Save As"),
		            wxString(""), wxString(""), wxString("VTK file (*.vtk)|*.vtk"),                   
                     wxSAVE|wxOVERWRITE_PROMPT, wxDefaultPosition );      

               if ( saveDialog->ShowModal() == wxID_OK )
               {
                  writeIsoSurface( dataset );
                  writeVtkThing( surface, (char*)(saveDialog->GetFilename()).c_str(), 1 );
                  surface->Delete();                  
                  txt->WriteText( "Saved as " + saveDialog->GetFilename() + "\n" );
               }
			   }
		   }
      
         else if ( wxID_CANCEL )txt->WriteText( wxString( "Cancel pressed... \n" ) );
         dataset->Delete();   
      }//for loop
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
	else if ( filNamesList->GetCount() ==0 ) txt->WriteText( wxString( "No files to be deleted \n" ) );
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

//-------------------------VTK geometry to STL
void makeFrame::writeVtkGeomToStl( vtkDataSet * vtkdataset, char filename [] )
{
   vtkTriangleFilter *tFilter = vtkTriangleFilter::New();
   vtkGeometryFilter *gFilter = NULL;

   // convert dataset to vtkPolyData 
   if ( vtkdataset->IsA("vtkPolyData") )
      tFilter->SetInput( (vtkPolyData*)vtkdataset );
   else 
   {
      gFilter = vtkGeometryFilter::New();
      gFilter->SetInput( vtkdataset );
      tFilter->SetInput( gFilter->GetOutput() );
   }

   
   
   vtkSTLWriter *writer = vtkSTLWriter::New();
      writer->SetInput( tFilter->GetOutput() );
      writer->SetFileName( filename );
      writer->SetFileTypeToBinary();
      writer->Write();
      writer->Delete();
   tFilter->Delete();
   if ( gFilter )      gFilter->Delete();   
}


//-------------------------making Iso-Surfaces
void makeFrame::writeIsoSurface( vtkDataSet* vtkdataset )
{
      activateScalar( vtkdataset );

      
      // Create an isosurace with the specified isosurface value...
      vtkContourFilter *contour = vtkContourFilter::New();
         contour->SetInput( vtkdataset );
         contour->SetValue( 0, popup->getIsoSurfValu() );
         contour->UseScalarTreeOff();
         vtkPolyDataNormals *normals = vtkPolyDataNormals::New();
         normals->SetInput( contour->GetOutput() );

      vtkGeometryFilter *filter = vtkGeometryFilter::New();
         filter->SetInput( normals->GetOutput() );
         
         filter->Update();
         surface = cfdGrid2Surface( filter->GetOutput(), popup->getDecimationValu() );
         contour->Delete();
         normals->Delete();
         filter->Delete();
}

#endif
