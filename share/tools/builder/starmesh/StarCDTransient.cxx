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
 * Date modified: $Date:  $
 * Version:       $Rev:  $
 * Author:        $Author:  $
 * Id:            $Id:  $
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

BEGIN_EVENT_TABLE( StarCDTranslator, wxFrame )

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

    wxStaticBox* inputDirSizer = new wxStaticBox(this, wxID_ANY, _("Input Directory"));
    wxStaticBoxSizer* inputDirGroup = new wxStaticBoxSizer(inputDirSizer, wxHORIZONTAL);
    frameSizer->Add(inputDirGroup, 1, wxALIGN_CENTER_HORIZONTAL|wxALL|wxEXPAND, 5);

    m_inputDir = new wxTextCtrl( this, INPUT_DIRECTORY, _("Add Input Directory"), wxDefaultPosition, wxDefaultSize, 0 );
    inputDirGroup->Add(m_inputDir,3, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    m_inputDirButton = new wxButton( this, INPUT_DIRECTORY_BUTTON, _("Browse..."), wxDefaultPosition, wxDefaultSize, 0 );
    inputDirGroup->Add(m_inputDirButton, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    wxStaticBox* outputDirSizer = new wxStaticBox(this, wxID_ANY, _("Static"));
    wxStaticBoxSizer* outputDirGroup = new wxStaticBoxSizer(outputDirSizer, wxHORIZONTAL);
    frameSizer->Add(outputDirGroup, 1, wxALIGN_CENTER_HORIZONTAL|wxALL|wxEXPAND, 5);

    m_outputDir = new wxTextCtrl( this, OUTPUT_DIRECTORY, _("Add Output Directory"), wxDefaultPosition, wxDefaultSize, 0 );
    outputDirGroup->Add(m_outputDir, 3, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    m_outputDirButton = new wxButton( this, OUTPUT_DIRECTORY_BUTTON, _("Browse..."), wxDefaultPosition, wxDefaultSize, 0 );
    outputDirGroup->Add(m_outputDirButton, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    wxBoxSizer* buttonSizer = new wxBoxSizer(wxHORIZONTAL);
    frameSizer->Add(buttonSizer, 1, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);

    m_translateButton = new wxButton( this, TRANSLATE_BUTTON, _("Translate Data"), wxDefaultPosition, wxDefaultSize, 0 );
    buttonSizer->Add(m_translateButton, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    m_closeButton = new wxButton( this, wxID_OK, _("Close"), wxDefaultPosition, wxDefaultSize, 0 );
    buttonSizer->Add(m_closeButton, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);
    
    SetAutoLayout(true); 
    frameSizer->Fit(this);
}
