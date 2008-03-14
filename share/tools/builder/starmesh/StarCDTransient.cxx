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
#include "StarCDTransient.h"

#include <wx/button.h>
#include <wx/radiobox.h>
#include <wx/sizer.h>
#include <wx/textctrl.h>
#include <wx/stattext.h>
#include <wx/statbox.h>
#include <wx/filedlg.h>
#include <wx/filename.h>
#include <wx/dirdlg.h>
#include <wx/dir.h>

BEGIN_EVENT_TABLE( StarCDTranslator, wxFrame )
    EVT_BUTTON(FILE_NAME_BUTTON, 	StarCDTranslator::FileName)
    EVT_BUTTON(OUTPUT_DIRECTORY_BUTTON, StarCDTranslator::OutputDirectory)
    EVT_BUTTON(TRANSLATE_BUTTON,	StarCDTranslator::Translate)
    EVT_BUTTON(QUIT_BUTTON,	 	StarCDTranslator::Quit)
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////////////////////
StarCDTranslator::StarCDTranslator( wxWindow* parent,
				    wxWindowID id, 
				    const wxString& title, 
				    const wxPoint& pos, 
				    const wxSize& size, 
				    long style )
:wxFrame(parent, -1, title, pos, size, style)
{
    //m_inputDirectory = 0;

    BuildGUI();
}
////////////////////////////////////////////////////////////////////////////////////////////////
StarCDTranslator::~StarCDTranslator()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////////////////////
void StarCDTranslator::BuildGUI()
{    
    wxBoxSizer* frameSizer = new wxBoxSizer(wxVERTICAL);
    this->SetSizer(frameSizer);

    wxBoxSizer* fileNameUnitsGroup = new wxBoxSizer(wxHORIZONTAL);
    frameSizer->Add(fileNameUnitsGroup, 1, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);

    wxStaticBox* fileNameGroupSizer = new wxStaticBox(this, wxID_ANY, _("StarCD File Name"));
    wxStaticBoxSizer* fileNameGroup = new wxStaticBoxSizer(fileNameGroupSizer, wxHORIZONTAL);
    fileNameUnitsGroup->Add(fileNameGroup, 3, wxALIGN_CENTER_VERTICAL|wxALL|wxEXPAND, 5);

    wxBoxSizer* fileNameGroupControls = new wxBoxSizer(wxHORIZONTAL);
    fileNameGroup->Add(fileNameGroupControls, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    m_fileName = new wxTextCtrl( this, FILE_NAME, _T(""), wxDefaultPosition, wxDefaultSize, 0 );
    fileNameGroupControls->Add(m_fileName, 3, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    m_fileNameButton = new wxButton( this, FILE_NAME_BUTTON, _("Browse..."), wxDefaultPosition, wxDefaultSize, 0 );
    fileNameGroupControls->Add(m_fileNameButton, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    wxString unitString[] = { _("SI"), _("English") };
    
    m_units = new wxRadioBox( this, UNITS_RADIOBOX, _("Units"), wxDefaultPosition, wxDefaultSize, 2, unitString, 1, wxRA_SPECIFY_COLS );
    m_units->SetSelection(0);
    fileNameUnitsGroup->Add(m_units, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    wxStaticBox* outputDirSizer = new wxStaticBox(this, wxID_ANY, _("Output Directory"));
    wxStaticBoxSizer* outputDirGroup = new wxStaticBoxSizer(outputDirSizer, wxHORIZONTAL);
    frameSizer->Add(outputDirGroup, 1, wxALIGN_CENTER_HORIZONTAL|wxALL|wxEXPAND, 5);

    m_outputDir = new wxTextCtrl( this, OUTPUT_DIRECTORY, _("Add Output Directory"), wxDefaultPosition, wxDefaultSize, 0 );
    outputDirGroup->Add(m_outputDir, 3, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    m_outputDirButton = new wxButton( this, OUTPUT_DIRECTORY_BUTTON, _("Browse..."), wxDefaultPosition, wxDefaultSize, 0 );
    outputDirGroup->Add(m_outputDirButton, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);
    
    wxStaticBox* dataGroupSizer = new wxStaticBox(this, wxID_ANY, _("Transient File Information"));
    wxStaticBoxSizer* dataGroup = new wxStaticBoxSizer(dataGroupSizer, wxHORIZONTAL);
    frameSizer->Add(dataGroup, 1, wxALIGN_CENTER_HORIZONTAL|wxALL|wxEXPAND, 5);
    
    wxBoxSizer* totalTimeSizer = new wxBoxSizer(wxVERTICAL);
    dataGroup->Add(totalTimeSizer, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    wxStaticText* totalTimeText = new wxStaticText( this, -1, _("Total Time Steps"), wxDefaultPosition, wxDefaultSize, 0 );
    totalTimeSizer->Add(totalTimeText, 1, wxALIGN_CENTER_HORIZONTAL|wxALL|wxADJUST_MINSIZE, 5);

    m_timeSteps = new wxTextCtrl( this, TIME_STEPS, _T(""), wxDefaultPosition, wxDefaultSize, 0 );
    totalTimeSizer->Add(m_timeSteps, 1, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);

    wxBoxSizer* beginningTimeSizer = new wxBoxSizer(wxVERTICAL);
    dataGroup->Add(beginningTimeSizer, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    wxStaticText* beginningTimeText = new wxStaticText( this, wxID_STATIC, _("Beginning Time Step"), wxDefaultPosition, wxDefaultSize, 0 );
    beginningTimeSizer->Add(beginningTimeText, 1, wxALIGN_CENTER_HORIZONTAL|wxALL|wxADJUST_MINSIZE, 5);

    m_startTime = new wxTextCtrl( this, BEGINNING_TIME, _T(""), wxDefaultPosition, wxDefaultSize, 0 );
    beginningTimeSizer->Add(m_startTime, 1, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);

    wxBoxSizer* postFrequencySizer = new wxBoxSizer(wxVERTICAL);
    dataGroup->Add(postFrequencySizer, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    wxStaticText* postFrequencyText = new wxStaticText( this, wxID_STATIC, _("Post Frequency"), wxDefaultPosition, wxDefaultSize, 0 );
    postFrequencySizer->Add(postFrequencyText, 1, wxALIGN_CENTER_HORIZONTAL|wxALL|wxADJUST_MINSIZE, 5);

    m_frequency = new wxTextCtrl( this, POST_FREQUENCY, _T(""), wxDefaultPosition, wxDefaultSize, 0 );
    postFrequencySizer->Add(m_frequency, 1, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);

    wxBoxSizer* buttonSizer = new wxBoxSizer(wxHORIZONTAL);
    frameSizer->Add(buttonSizer, 1, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);

    m_translateButton = new wxButton( this, TRANSLATE_BUTTON, _("Translate Data"), wxDefaultPosition, wxDefaultSize, 0 );
    buttonSizer->Add(m_translateButton, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    m_closeButton = new wxButton(this, QUIT_BUTTON, wxT("Quit"));
    buttonSizer->Add(m_closeButton, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);
    
    SetAutoLayout(true); 
    frameSizer->Fit(this);
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////
void StarCDTranslator::FileName( wxCommandEvent& event )
{
    wxPoint pos( 0, 0 );
    wxFileDialog dialog( this,
                         _T( "Open file" ),
                         _T( "" ),
                         _T( "" ),
                         _T( "StarCD Post Files (*.ccmt)|*.ccmt;|All Files (*.*)|*.*" ),
                         wxOPEN | wxFILE_MUST_EXIST,
                         wxDefaultPosition );
    dialog.CentreOnParent();
    
    if( dialog.ShowModal() == wxID_OK )
    {
        wxFileName datasetFilename( dialog.GetPath() );
	wxString filename = datasetFilename.GetName();
	m_fileName->SetValue( filename );
    }
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////
void StarCDTranslator::OutputDirectory( wxCommandEvent& event )
{
     wxDirDialog m_outputDirectory(this, _T("Choose a directory"), ::wxGetCwd(), wxDD_DEFAULT_STYLE);

     if ( m_outputDirectory.ShowModal() == wxID_OK )
     {
          m_outputString = wxString(m_outputDirectory.GetPath().c_str(), wxConvUTF8);
          m_outputDir->SetValue(m_outputString);
     }
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////
void StarCDTranslator::Translate( wxCommandEvent& event )
{
    std::string filename( static_cast< const char* >( wxConvCurrent->cWX2MB( m_fileName->GetValue().c_str() ) ) );
    std::string units( static_cast< const char* >( wxConvCurrent->cWX2MB( m_units->GetStringSelection().c_str() ) ) );
    std::string outputDir( static_cast< const char* >( wxConvCurrent->cWX2MB( m_outputDir->GetValue().c_str() ) ) );

    long int temp1;
    m_timeSteps->GetValue().ToLong( &temp1 );
    long int temp2;
    m_startTime->GetValue().ToLong( &temp2 );
    long int temp3;
    m_frequency->GetValue().ToLong( &temp3 );

    m_transient->writeScript( filename, units, temp1, temp2, temp3 );
    m_transient->writeStarParam( filename, temp1, temp3 );
    m_transient->writeTranslatorScript( filename, outputDir, temp1, temp3 );
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////
void StarCDTranslator::Quit( wxCommandEvent& event)
{
    Destroy();
    exit(0);
}
