/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *   - National Energy Technology Laboratory, www.netl.doe.gov
 *   - West Virginia Virtual Environments Laboratory, wvvel.csee.wvu.edu
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
 * File:          $RCSfile: mFixTranslator.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

// File: mFixTranslator.cpp
// Authors: Jeremy Jarrell
//		   jarrell@csee.wvu.edu
//          Jim Canon
//		   jcanon@csee.wvu.edu
//          West Virginia Virtual Environments Lab
// Date: Spring 2004
//
// This is a portion of the GUI written for the CFD Translator developed on contract from DOE-NETL.
//

#include "mFixTranslator.h"

mFixTranslator::mFixTranslator(wxWindow* parent, int id, const wxString& title, const wxPoint& pos, const wxSize& size, long style):
    wxDialog(parent, id, title, pos, size, wxDEFAULT_DIALOG_STYLE)
{

    // mFixTranslator::mFixTranslator
    FileOpenBttn = new wxButton(this, BUTTON_FILE_OPEN, wxT("Open Restart File"));
    DestBttn = new wxButton(this, BUTTON_DEST, wxT("Output Destination"));
    AboutBttn = new wxButton(this, BUTTON_ABOUT, wxT("About"));
    static_line_4 = new wxStaticLine(this, -1);

	// Create text controls for file names and time steps
    SPGauge_1 = new wxGauge(this, GAUGE_SP1, 100, wxDefaultPosition, wxSize(65,20), wxGA_HORIZONTAL);
	static_text_1 = new wxStaticText(this, -1, wxT("             SP1"), wxDefaultPosition, wxDefaultSize, 0, wxT(""));
	static_text_1_TS = new wxStaticText(this, -1, wxT("0/0 Timesteps"), wxDefaultPosition, wxDefaultSize, 0, wxT(""));

    SPGauge_2 = new wxGauge(this, GAUGE_SP2, 100, wxDefaultPosition, wxSize(65,20), wxGA_HORIZONTAL);
	static_text_2 = new wxStaticText(this, -1, wxT("             SP2"), wxDefaultPosition, wxDefaultSize, 0, wxT(""));
	static_text_2_TS = new wxStaticText(this, -1, wxT("0/0 Timesteps"), wxDefaultPosition, wxDefaultSize, 0, wxT(""));
    
    SPGauge_3 = new wxGauge(this, GAUGE_SP3, 100, wxDefaultPosition, wxSize(65,20), wxGA_HORIZONTAL);
	static_text_3 = new wxStaticText(this, -1, wxT("             SP3"), wxDefaultPosition, wxDefaultSize, 0, wxT(""));
	static_text_3_TS = new wxStaticText(this, -1, wxT("0/0 Timesteps"), wxDefaultPosition, wxDefaultSize, 0, wxT(""));
	
    SPGauge_4 = new wxGauge(this, GAUGE_SP4, 100, wxDefaultPosition, wxSize(65,20), wxGA_HORIZONTAL);
	static_text_4 = new wxStaticText(this, -1, wxT("             SP4"), wxDefaultPosition, wxDefaultSize, 0, wxT(""));
	static_text_4_TS = new wxStaticText(this, -1, wxT("0/0 Timesteps"), wxDefaultPosition, wxDefaultSize, 0, wxT(""));
	
    SPGauge_5 = new wxGauge(this, GAUGE_SP5, 100, wxDefaultPosition, wxSize(65,20), wxGA_HORIZONTAL);
	static_text_5 = new wxStaticText(this, -1, wxT("             SP5"), wxDefaultPosition, wxDefaultSize, 0, wxT(""));
	static_text_5_TS = new wxStaticText(this, -1, wxT("0/0 Timesteps"), wxDefaultPosition, wxDefaultSize, 0, wxT(""));
    
    SPGauge_6 = new wxGauge(this, GAUGE_SP6, 100, wxDefaultPosition, wxSize(65,20), wxGA_HORIZONTAL);
	static_text_6 = new wxStaticText(this, -1, wxT("             SP6"), wxDefaultPosition, wxDefaultSize, 0, wxT(""));
	static_text_6_TS = new wxStaticText(this, -1, wxT("0/0 Timesteps"), wxDefaultPosition, wxDefaultSize, 0, wxT(""));
    
    SPGauge_7 = new wxGauge(this, GAUGE_SP7, 100, wxDefaultPosition, wxSize(65,20), wxGA_HORIZONTAL);
	static_text_7 = new wxStaticText(this, -1, wxT("             SP7"), wxDefaultPosition, wxDefaultSize, 0, wxT(""));
	static_text_7_TS = new wxStaticText(this, -1, wxT("0/0 Timesteps"), wxDefaultPosition, wxDefaultSize, 0, wxT(""));
    
    SPGauge_8 = new wxGauge(this, GAUGE_SP8, 100, wxDefaultPosition, wxSize(65,20), wxGA_HORIZONTAL);
	static_text_8 = new wxStaticText(this, -1, wxT("             SP8"), wxDefaultPosition, wxDefaultSize, 0, wxT(""));
	static_text_8_TS = new wxStaticText(this, -1, wxT("0/0 Timesteps"), wxDefaultPosition, wxDefaultSize, 0, wxT(""));
    
    SPGauge_9 = new wxGauge(this, GAUGE_SP9, 100, wxDefaultPosition, wxSize(65,20), wxGA_HORIZONTAL);
	static_text_9 = new wxStaticText(this, -1, wxT("             SP9"), wxDefaultPosition, wxDefaultSize, 0, wxT(""));
	static_text_9_TS = new wxStaticText(this, -1, wxT("0/0 Timesteps"), wxDefaultPosition, wxDefaultSize, 0, wxT(""));
    
    SPGauge_a = new wxGauge(this, GAUGE_SPa, 100, wxDefaultPosition, wxSize(65,20), wxGA_HORIZONTAL);
	static_text_a = new wxStaticText(this, -1, wxT("             SPA"), wxDefaultPosition, wxDefaultSize, 0, wxT(""));
	static_text_a_TS = new wxStaticText(this, -1, wxT("0/0 Timesteps"), wxDefaultPosition, wxDefaultSize, 0, wxT(""));
 
	static_line_1 = new wxStaticLine(this, -1);
	statusTitle = new wxStaticText(this, -1, wxT("Status Information:"));
	text_ctrl_statusReport = new wxTextCtrl(this, -1, wxT(""), wxDefaultPosition, wxSize(300,200), wxTE_MULTILINE|wxTE_READONLY|wxTE_RICH|wxTE_DONTWRAP|wxHSCROLL);
	enableLoggingCheckbox = new wxCheckBox(this, -1, wxT("Enable logging"));

	// Output file(s) wxCheckBox group
	// (these checkbox are contained in checkboxGroupSizer)
	legacyVTKCheckbox = new wxCheckBox(this, CHECKBOX_VTK, wxT("Legacy VTK"));
	const wxString legacyVTKRadioBox_choices[] = {
        	wxT("ASCII Ouput"),
        	wxT("Binary Ouput")
    	};
	
	legacyVTKRadioBox = new wxRadioBox(this, RADIOBOX_VTK, wxT("Legacy Output Format:"), wxDefaultPosition, wxDefaultSize, 2, 
								legacyVTKRadioBox_choices, 0, wxRA_SPECIFY_COLS);
	serialXMLCheckbox = new wxCheckBox(this, CHECKBOX_SERIAL_XML, wxT("Serial XML"));
	parallelXMLCheckbox = new wxCheckBox(this, CHECKBOX_PARALLEL_XML, wxT("Parallel XML"));
	cgnsCheckbox = new wxCheckBox(this, CHECKBOX_CGNS, wxT("CGNS"));
	selectAllButton = new wxButton(this, BUTTON_SELECTALL, wxT("Select All")); 
	unselectAllButton = new wxButton(this, BUTTON_UNSELECTALL, wxT("Unselect All"));
	
	static_line_6 = new wxStaticLine(this, -1, wxDefaultPosition, wxDefaultSize, wxLI_HORIZONTAL);
	static_line_7 = new wxStaticLine(this, -1, wxDefaultPosition, wxDefaultSize, wxLI_VERTICAL);
	static_line_8 = new wxStaticLine(this, -1, wxDefaultPosition, wxDefaultSize, wxLI_VERTICAL);
	
	// Options group
	static_line_11 = new wxStaticLine(this, -1, wxDefaultPosition, wxDefaultSize, wxLI_HORIZONTAL);
	static_line_options = new wxStaticLine(this, -1, wxDefaultPosition, wxDefaultSize, wxLI_HORIZONTAL);
	exportGeometry = new wxCheckBox(this, -1, wxT("Export Geometry"));
	exportBoundingBox = new wxCheckBox(this, -1, wxT("Export Bounding Box"));
        selectAllOptions = new wxButton(this, BUTTON_SELECTALL_OPTIONS, wxT("Select All"));
        unselectAllOptions = new wxButton(this, BUTTON_UNSELECTALL_OPTIONS, wxT("Unselect All"));

	// Restart file information wxTextCtrl group
	// (these objects are containes in ResInfoGridSizer)
	ST_RunName = new wxStaticText(this, -1, wxT("Run Name:"), wxDefaultPosition, wxDefaultSize, 0);
	TC_RunName = new wxTextCtrl(this, -1, wxT(""), wxDefaultPosition, wxDefaultSize, 0);
	ST_RunDate = new wxStaticText(this, -1, wxT("Run Date:"), wxDefaultPosition, wxDefaultSize, 0);
	TC_RunDate = new wxTextCtrl(this, -1, wxT(""), wxDefaultPosition, wxDefaultSize, 0);
	ST_RunTime = new wxStaticText(this, -1, wxT("Run Time:"), wxDefaultPosition, wxDefaultSize, 0);
	TC_RunTime = new wxTextCtrl(this, -1, wxT(""), wxDefaultPosition, wxDefaultSize, 0);
	ST_VersionNum = new wxStaticText(this, -1, wxT("Version Number:"), wxDefaultPosition, wxDefaultSize, 0);
	TC_VersionNum = new wxTextCtrl(this, -1, wxT(""), wxDefaultPosition, wxDefaultSize, 0);
	ST_CoordinateSystem = new wxStaticText(this, -1, wxT("Coordinate System:"), wxDefaultPosition, wxDefaultSize, 0);
	TC_CoordinateSystem = new wxTextCtrl(this, -1, wxT(""), wxDefaultPosition, wxDefaultSize, 0);
	ST_GasSpecies = new wxStaticText(this, -1, wxT("Gas Species:"), wxDefaultPosition, wxDefaultSize, 0);
	TC_GasSpecies = new wxTextCtrl(this, -1, wxT(""), wxDefaultPosition, wxDefaultSize, 0);
	ST_SolidPhases = new wxStaticText(this, -1, wxT("Solid Phases:"), wxDefaultPosition, wxDefaultSize, 0);
	TC_SolidPhases = new wxTextCtrl(this, -1, wxT(""), wxDefaultPosition, wxDefaultSize, 0);
	ST_NumScalars = new wxStaticText(this, -1, wxT("Number of Scalars:"), wxDefaultPosition, wxDefaultSize, 0);
	TC_NumScalars = new wxTextCtrl(this, -1, wxT(""), wxDefaultPosition, wxDefaultSize, 0);
	ST_NumReactionRates = new wxStaticText(this, -1, wxT("Number of Reaction Rates:"), wxDefaultPosition, wxDefaultSize, 0);
	TC_NumReactionRates = new wxTextCtrl(this, -1, wxT(""), wxDefaultPosition, wxDefaultSize, 0);
	ST_IMax = new wxStaticText(this, -1, wxT("i max:"), wxDefaultPosition, wxDefaultSize, 0);
	TC_IMax = new wxTextCtrl(this, -1, wxT(""), wxDefaultPosition, wxDefaultSize, 0);
	ST_JMax = new wxStaticText(this, -1, wxT("j max:"), wxDefaultPosition, wxDefaultSize, 0);
	TC_JMax = new wxTextCtrl(this, -1, wxT(""), wxDefaultPosition, wxDefaultSize, 0);
	ST_KMax = new wxStaticText(this, -1, wxT("k max:"), wxDefaultPosition, wxDefaultSize, 0);
	TC_KMax = new wxTextCtrl(this, -1, wxT(""), wxDefaultPosition, wxDefaultSize, 0);

	// Output variables checkbox group
	VoidFractionVariable = new wxCheckBox(this, CHECKBOX_VOIDFRACTION, wxT("Void Fraction"));
	GasPressureVariable = new wxCheckBox(this, CHECKBOX_GASPRESSURE, wxT("Gas Pressure"));
	P_StarVariable = new wxCheckBox(this, CHECKBOX_P_STAR, wxT("P Star"));
	GasVelocityVariable = new wxCheckBox(this, CHECKBOX_GASVELOCITY, wxT("GasVelocity"));
	SolidPhaseVelocityVariable = new wxCheckBox(this, CHECKBOX_SOLIDPHASEVELOCITY, wxT("Solid Phase Velocity"));
	SolidPhaseDensityVariable = new wxCheckBox(this, CHECKBOX_SOLIDPHASEDENSITY, wxT("Solid Phase Density"));
	TemperaturesVariable = new wxCheckBox(this, CHECKBOX_TEMPERATURE, wxT("Temperatures"));
	GasSpeciesVariable = new wxCheckBox(this, CHECKBOX_GASSPECIES, wxT("Gas Species"));
	SolidPhaseSpeciesVariable = new wxCheckBox(this, CHECKBOX_SOLIDPHASESPECIES, wxT("Solid Phase Species"));
	Theta_MVariable = new wxCheckBox(this, CHECKBOX_THETA_M, wxT("Theta M"));
	ScalarsVariable = new wxCheckBox(this, CHECKBOX_SCALARS, wxT("Scalars"));
	NumReactionRatesVariable = new wxCheckBox(this, CHECKBOX_NUMREACTIONRATES, wxT("Number of Reaction Rates"));
	static_line_9 = new wxStaticLine(this, -1);
	SelectAllOutputVariables = new wxButton(this, BUTTON_SELECTALLOUTPUTVARS, wxT("Select All"));
	UnselectAllOutputVariables = new wxButton(this, BUTTON_UNSELECTALLOUTPUTVARS, wxT("Unselect All"));

	BeginTSBox = new wxTextCtrl(this, TEXTCTRL_BEGINNINGTS, wxT(""), wxDefaultPosition, wxSize(72,20));
	BeginTSLabel = new wxStaticText(this, -1, wxT("Starting Timestep:"));
	EndTSBox = new wxTextCtrl(this, TEXTCTRL_ENDINGTS, wxT(""), wxDefaultPosition, wxSize(72,20));
	EndTSLabel = new wxStaticText(this, -1, wxT("Ending Timestep:"));
	static_line_10 = new wxStaticLine(this, -1);
	EnableSpecificTSRange = new wxCheckBox(this, CHECKBOX_ENABLESPECIFICTSRANGE, wxT("Enable Output Range"));
	
	static_line_3 = new wxStaticLine(this, -1);
	TotalProgress = new wxStaticText(this, -1, wxT("Total Progress:"), wxDefaultPosition, wxDefaultSize, 0);
	ProgressGauge = new wxGauge(this, -1, 100, wxDefaultPosition, wxDefaultSize, wxGA_HORIZONTAL);
	outputFormatText = new wxStaticText(this, -1, wxT(""), wxDefaultPosition, wxDefaultSize, 0);
	static_line_2 = new wxStaticLine(this, -1);
	static_line_5 = new wxStaticLine(this, -1);
	GoBttn = new wxButton(this, BUTTON_GO, wxT("Go"));
	CancelBttn = new wxButton(this, BUTTON_CANCEL, wxT("Close"));

	// Create array for output variable flags
	resH.varSelectFlag = new int[12];

    set_properties();
    do_layout();
}

void mFixTranslator::set_properties()
{
    // mFixTranslator::set_properties
    SetTitle(wxT("mFix File Translator"));
    FileOpenBttn->SetToolTip(wxT("Choose the Restart file that you wish to translate"));
    DestBttn->SetToolTip(wxT("Choose the output directory"));
	legacyVTKCheckbox->SetToolTip(wxT("Legacy VTK"));
	legacyVTKRadioBox->SetToolTip(wxT("ASCII or Binary File Format"));
	legacyVTKRadioBox->SetSelection(1);
	serialXMLCheckbox->SetToolTip(wxT("Serial XML"));
	parallelXMLCheckbox->SetToolTip(wxT("Parallel XML"));
	cgnsCheckbox->SetToolTip(wxT("CGNS"));
	selectAllButton->SetToolTip(wxT("Select all file formats for output"));
	unselectAllButton->SetToolTip(wxT("Clear selection"));
    	AboutBttn->SetToolTip(wxT("Display program information"));
	
	// Set SP_Gauge* Tooltips
	SPGauge_1->SetToolTip(wxT("Number of timesteps remaining to be processed in sp1"));
	SPGauge_2->SetToolTip(wxT("Number of timesteps remaining to be processed in sp2"));
	SPGauge_3->SetToolTip(wxT("Number of timesteps remaining to be processed in sp3"));
	SPGauge_4->SetToolTip(wxT("Number of timesteps remaining to be processed in sp4"));
	SPGauge_5->SetToolTip(wxT("Number of timesteps remaining to be processed in sp5"));
	SPGauge_6->SetToolTip(wxT("Number of timesteps remaining to be processed in sp6"));
	SPGauge_7->SetToolTip(wxT("Number of timesteps remaining to be processed in sp7"));
	SPGauge_8->SetToolTip(wxT("Number of timesteps remaining to be processed in sp8"));
	SPGauge_9->SetToolTip(wxT("Number of timesteps remaining to be processed in sp9"));
	SPGauge_a->SetToolTip(wxT("Number of timesteps remaining to be processed in spa"));

	text_ctrl_statusReport->SetBackgroundColour(wxColour(194, 191, 165));
    	text_ctrl_statusReport->SetToolTip(wxT("Status report"));
	enableLoggingCheckbox->SetToolTip(wxT("Save status report to disk"));
 
	// Set restart file information properties
	TC_RunName->SetBackgroundColour(wxColour(194, 191, 165));
	TC_RunDate->SetBackgroundColour(wxColour(194, 191, 165));
	TC_RunTime->SetBackgroundColour(wxColour(194, 191, 165));
	TC_VersionNum->SetBackgroundColour(wxColour(194, 191, 165));
	TC_CoordinateSystem->SetBackgroundColour(wxColour(194, 191, 165));
	TC_GasSpecies->SetBackgroundColour(wxColour(194, 191, 165));
	TC_SolidPhases->SetBackgroundColour(wxColour(194, 191, 165));
	TC_NumScalars->SetBackgroundColour(wxColour(194, 191, 165));
	TC_NumReactionRates->SetBackgroundColour(wxColour(194, 191, 165));
	TC_IMax->SetBackgroundColour(wxColour(194, 191, 165));
	TC_JMax->SetBackgroundColour(wxColour(194, 191, 165));
	TC_KMax->SetBackgroundColour(wxColour(194, 191, 165));
	TC_RunName->SetToolTip(wxT("Run Name"));
	TC_RunDate->SetToolTip(wxT("Run Date"));
	TC_RunTime->SetToolTip(wxT("Run Time"));
	TC_VersionNum->SetToolTip(wxT("Version Number"));
	TC_CoordinateSystem->SetToolTip(wxT("Coordinate System"));
	TC_GasSpecies->SetToolTip(wxT("Gas Species"));
	TC_SolidPhases->SetToolTip(wxT("Solid Phases"));
	TC_NumScalars->SetToolTip(wxT("Number of Scalars"));
	TC_NumReactionRates->SetToolTip(wxT("Number of Reaction Rates"));
	TC_IMax->SetToolTip(wxT("i max"));
	TC_IMax->SetToolTip(wxT("j max"));
	TC_IMax->SetToolTip(wxT("k max"));

	legacyVTKCheckbox->Enable(FALSE);
	legacyVTKRadioBox->Enable(FALSE);
	serialXMLCheckbox->Enable(FALSE);
	parallelXMLCheckbox->Enable(FALSE);
	cgnsCheckbox->Enable(FALSE);
	selectAllButton->Enable(FALSE);
	unselectAllButton->Enable(FALSE);

	exportGeometry->Enable(FALSE);
	exportGeometry->SetValue(FALSE);
	exportBoundingBox->Enable(FALSE);
	exportBoundingBox->SetValue(FALSE);
	selectAllOptions->Enable(FALSE);
	unselectAllOptions->Enable(FALSE);

	VoidFractionVariable->SetValue(FALSE);
	VoidFractionVariable->Enable(FALSE);
	VoidFractionVariable->SetToolTip(wxT("Enable output of Void Fraction"));
	GasPressureVariable->SetValue(FALSE);
	GasPressureVariable->Enable(FALSE);
	GasPressureVariable->SetToolTip(wxT("Enable output of Gas Pressure"));
	P_StarVariable->SetValue(FALSE);
	P_StarVariable->Enable(FALSE);
	P_StarVariable->SetToolTip(wxT("Enable output of P Star"));
	GasVelocityVariable->SetValue(FALSE);
	GasVelocityVariable->Enable(FALSE);
	GasVelocityVariable->SetToolTip(wxT("Enable output of Gas Velocity"));
	SolidPhaseVelocityVariable->SetValue(FALSE);
	SolidPhaseVelocityVariable->Enable(FALSE);
	SolidPhaseVelocityVariable->SetToolTip(wxT("Enable output of Solid Phase Velocity"));
	SolidPhaseDensityVariable->SetValue(FALSE);
	SolidPhaseDensityVariable->Enable(FALSE);
	SolidPhaseDensityVariable->SetToolTip(wxT("Enable output of Solid Phase Density"));
	TemperaturesVariable->SetValue(FALSE);
	TemperaturesVariable->Enable(FALSE);
	TemperaturesVariable->SetToolTip(wxT("Enable output of Temperatures"));
	GasSpeciesVariable->SetValue(FALSE);
	GasSpeciesVariable->Enable(FALSE);
	GasSpeciesVariable->SetToolTip(wxT("Enable output of GasSpecies"));
	SolidPhaseSpeciesVariable->SetValue(FALSE);
	SolidPhaseSpeciesVariable->Enable(FALSE);
	SolidPhaseSpeciesVariable->SetToolTip(wxT("Enable output of Solid Phase Species"));
	Theta_MVariable->SetValue(FALSE);
	Theta_MVariable->Enable(FALSE);
	Theta_MVariable->SetToolTip(wxT("Enable output of Theta M"));
	ScalarsVariable->SetValue(FALSE);
	ScalarsVariable->Enable(FALSE);
	ScalarsVariable->SetToolTip(wxT("Enable output of Scalars"));
	NumReactionRatesVariable->SetValue(FALSE);
	NumReactionRatesVariable->Enable(FALSE);
	NumReactionRatesVariable->SetToolTip(wxT("Enable output of Number of Reaction Rates"));
	SelectAllOutputVariables->Enable(FALSE);
	SelectAllOutputVariables->SetToolTip(wxT("Select all available variables for output"));
	UnselectAllOutputVariables->Enable(FALSE);
	UnselectAllOutputVariables->SetToolTip(wxT("Clear selection"));
	
	sp1Active = FALSE;
	sp2Active = FALSE;
	sp3Active = FALSE;
	sp4Active = FALSE;
	sp5Active = FALSE;
	sp6Active = FALSE;
	sp7Active = FALSE;
	sp8Active = FALSE;
	sp9Active = FALSE;
	spaActive = FALSE;

	BeginTSLabel->Enable(FALSE);
	BeginTSBox->Enable(FALSE);
	BeginTSBox->SetToolTip(wxT("Enter a beginning timestep for output"));
	EndTSBox->Enable(FALSE);
	EndTSBox->SetToolTip(wxT("Enter an ending timestep for output"));
	EndTSLabel->Enable(FALSE);
	EnableSpecificTSRange->Enable(FALSE);
	EnableSpecificTSRange->SetToolTip(wxT("Enable a specific range of timesteps for output"));

	enableLoggingCheckbox->Enable(FALSE);
	EnableSpecificTSRange->SetValue(FALSE);

	ProgressGauge->SetSize(wxSize(225, 30));
    	ProgressGauge->SetToolTip(wxT("Translation progress"));
	ProgressGauge->SetForegroundColour(wxColour(0,0,255));
    
	GoBttn->Enable(FALSE);
	GoBttn->SetToolTip(wxT("Begin translation"));
    	CancelBttn->SetToolTip(wxT("Stop translation"));

	// Setup initial output flags
	legacyVTKOutput = TRUE;
	serialXMLOutput = FALSE;
	parallelXMLOutput = FALSE;
	CGNSOutput = FALSE;
	legacyVTKCheckbox->SetValue(TRUE);
	GridWriterFormatFlag = 2;

	// Set icon
	wxIcon icon;
	icon.CopyFromBitmap(wxBitmap(wxT("res/vel.ico"), wxBITMAP_TYPE_ANY));
	SetIcon(icon);
}


void mFixTranslator::do_layout()
{
	wxBoxSizer* sizer_7 = new wxBoxSizer(wxVERTICAL);
	wxBoxSizer* sizer_10 = new wxBoxSizer(wxHORIZONTAL);
	wxFlexGridSizer* grid_sizer_1 = new wxFlexGridSizer(0, 10, 0, 0);
	wxBoxSizer* sizer_8 = new wxBoxSizer(wxHORIZONTAL);
	wxBoxSizer* sizer_11 = new wxBoxSizer(wxHORIZONTAL);
	wxBoxSizer* sizer_13 = new wxBoxSizer(wxHORIZONTAL);
	wxBoxSizer* sizer_select_options = new wxBoxSizer(wxHORIZONTAL);
	
	wxStaticBox* outputVariablesGroup = new wxStaticBox(this, -1, wxT("Variables Selected for Output"), wxDefaultPosition,															wxDefaultSize, 0, wxT("Variables Selected for Output"));
	wxStaticBoxSizer* outputVariablesSizer = new wxStaticBoxSizer(outputVariablesGroup, wxVERTICAL);
	
	wxStaticBox* optionsGroup = new wxStaticBox(this, -1, wxT("Miscellaneous Options"), wxDefaultPosition, wxDefaultSize, 0, wxT("Miscellaneous Options"));
	wxStaticBoxSizer* optionsGroupSizer = new wxStaticBoxSizer(optionsGroup, wxVERTICAL);	

	wxStaticBox* enableSpecificTSRangeGroup = new wxStaticBox(this, -1, wxT(""), wxDefaultPosition, wxDefaultSize, 0, wxT(""));
	wxStaticBoxSizer* enableSpecificTSRangeSizer = new wxStaticBoxSizer(enableSpecificTSRangeGroup, wxVERTICAL);

	wxBoxSizer* sizer_12 = new wxBoxSizer(wxVERTICAL);
	wxFlexGridSizer* outputGridSizer = new wxFlexGridSizer(0, 2, 0, 0);
	wxStaticBox* checkboxGroup = new wxStaticBox(this, -1, wxT("Output file formats"), wxDefaultPosition,
													wxDefaultSize, 0, wxT("Output file formats"));
	wxStaticBoxSizer* checkboxGroupSizer = new wxStaticBoxSizer(checkboxGroup, wxVERTICAL);
	wxBoxSizer* sizer_file_dialogs = new wxBoxSizer(wxHORIZONTAL);
	
	wxStaticBox* informationGroup = new wxStaticBox(this, -1, wxT("Restart Information"), wxDefaultPosition, wxDefaultSize, 0, wxT("Restart Information"));	
	wxStaticBoxSizer* informationGroupSizer = new wxStaticBoxSizer(informationGroup, wxVERTICAL);	
	wxFlexGridSizer* ResInfoGridSizer = new wxFlexGridSizer(0, 2, 0, 0);
	wxBoxSizer* sizer_mid_horiz = new wxBoxSizer(wxHORIZONTAL);
	wxBoxSizer* sizer_mid_vert = new wxBoxSizer(wxVERTICAL);
	wxBoxSizer* sizer_mid_vert2 = new wxBoxSizer(wxVERTICAL);
	
	wxBoxSizer* dummySizer1 = new wxBoxSizer(wxHORIZONTAL);
	wxBoxSizer* dummySizer2 = new wxBoxSizer(wxHORIZONTAL);
	wxBoxSizer* dummySizer3 = new wxBoxSizer(wxHORIZONTAL);

	sizer_8->Add(sizer_file_dialogs, 0, wxALIGN_CENTER_HORIZONTAL, 3);
	sizer_file_dialogs->Add(5, 0, 0, 0);
	sizer_file_dialogs->Add(FileOpenBttn, 0, wxALL|wxALIGN_CENTER_VERTICAL|wxALIGN_CENTER_HORIZONTAL, 4);
	sizer_file_dialogs->Add(5, 0, 0, 0);
	sizer_file_dialogs->Add(DestBttn, 0, wxALL|wxALIGN_CENTER_VERTICAL|wxALIGN_CENTER_HORIZONTAL, 4);
	sizer_file_dialogs->Add(449, 0, 0, 0);
	sizer_file_dialogs->Add(AboutBttn, 0, wxALL|wxALIGN_CENTER_VERTICAL|wxALIGN_CENTER_HORIZONTAL, 4);
	sizer_7->Add(sizer_8, 0, 0, 0);
	sizer_7->Add(static_line_4, 0, wxALL|wxEXPAND, 0);
	sizer_7->Add(grid_sizer_1, 1, wxEXPAND, 0);

	grid_sizer_1->Add(static_text_1, 0, wxALL, 3); 
	grid_sizer_1->Add(SPGauge_1, 0, wxALL, 3);
	grid_sizer_1->Add(static_text_1_TS, 0, wxALL, 3);

	grid_sizer_1->Add(static_text_5, 0, wxALL, 3);
	grid_sizer_1->Add(SPGauge_5, 0, wxALL, 3);
	grid_sizer_1->Add(static_text_5_TS, 0, wxALL, 3);
	
	grid_sizer_1->Add(static_text_9, 0, wxALL, 3);
	grid_sizer_1->Add(SPGauge_9, 0, wxALL, 3);
	grid_sizer_1->Add(static_text_9_TS, 0, wxALL, 3);

	grid_sizer_1->Add(10, 0, 0, 0);

	grid_sizer_1->Add(static_text_2, 0, wxALL, 3);
	grid_sizer_1->Add(SPGauge_2, 0, wxALL, 3);
	grid_sizer_1->Add(static_text_2_TS, 0, wxALL, 3);

	grid_sizer_1->Add(static_text_6, 0, wxALL, 3);
	grid_sizer_1->Add(SPGauge_6, 0, wxALL, 3);
	grid_sizer_1->Add(static_text_6_TS, 0, wxALL, 3);

	grid_sizer_1->Add(static_text_a, 0, wxALL, 3);
	grid_sizer_1->Add(SPGauge_a, 0, wxALL, 3);
	grid_sizer_1->Add(static_text_a_TS, 0, wxALL, 3);

	grid_sizer_1->Add(10, 0, 0, 0);

	grid_sizer_1->Add(static_text_3, 0, wxALL, 3);
	grid_sizer_1->Add(SPGauge_3, 0, wxALL, 3);
	grid_sizer_1->Add(static_text_3_TS, 0, wxALL, 3);

	grid_sizer_1->Add(static_text_7, 0, wxALL, 3);
	grid_sizer_1->Add(SPGauge_7, 0, wxALL, 3);
	grid_sizer_1->Add(static_text_7_TS, 0, wxALL, 3);

	grid_sizer_1->Add(dummySizer1, 0, wxALL, 3);
	grid_sizer_1->Add(dummySizer2, 0, wxALL, 3);
	grid_sizer_1->Add(dummySizer3, 0, wxALL, 3);

	grid_sizer_1->Add(10, 0, 0, 0);
	
	grid_sizer_1->Add(static_text_4, 0, wxALL, 3);
	grid_sizer_1->Add(SPGauge_4, 0, wxALL, 3);
	grid_sizer_1->Add(static_text_4_TS, 0, wxALL, 3);
	
	grid_sizer_1->Add(static_text_8, 0, wxALL, 3);
	grid_sizer_1->Add(SPGauge_8, 0, wxALL, 3);
	grid_sizer_1->Add(static_text_8_TS, 0, wxALL, 3);

	sizer_7->Add(static_line_1, 0, wxALL|wxEXPAND, 3);
	sizer_7->Add(sizer_mid_horiz, 0, 0, 0);
	sizer_mid_horiz->Add(sizer_mid_vert, 0, 0, 0);
	sizer_mid_vert->Add(checkboxGroupSizer, 0, wxALL|wxEXPAND, 0);
	checkboxGroupSizer->Add(legacyVTKCheckbox, 0, wxALL, 3);
	checkboxGroupSizer->Add(legacyVTKRadioBox, 0, wxALL|wxALIGN_CENTER, 3);
	checkboxGroupSizer->Add(serialXMLCheckbox, 0, wxALL, 3);
	checkboxGroupSizer->Add(parallelXMLCheckbox, 0, wxALL, 3);
	checkboxGroupSizer->Add(cgnsCheckbox, 0, wxALL, 3);
	checkboxGroupSizer->Add(static_line_6, 0, wxEXPAND, 0);
	checkboxGroupSizer->Add(sizer_11, 0, wxALIGN_CENTER_HORIZONTAL, 3);
	sizer_11->Add(selectAllButton, 0, wxALL|wxALIGN_CENTER_HORIZONTAL, 3);
	sizer_11->Add(10, 5, 0, 0);
	sizer_11->Add(unselectAllButton, 0, wxALL|wxALIGN_CENTER_HORIZONTAL, 3);
	
	//sizer_mid_vert->Add(static_line_5, 0, wxALL|wxEXPAND, 3);
	sizer_mid_vert->Add(0,30);
	sizer_mid_vert->Add(statusTitle, 0, wxALL|wxALIGN_LEFT, 2);
    	sizer_mid_vert->Add(text_ctrl_statusReport, 0, wxALL|wxEXPAND|wxALIGN_BOTTOM, 3);
	sizer_mid_vert->Add(enableLoggingCheckbox, 0, wxALL|wxALIGN_RIGHT, 2);

	sizer_mid_horiz->Add(5, 0);
	sizer_mid_horiz->Add(static_line_7, 0, wxEXPAND|wxBOTTOM, 2);
	sizer_mid_horiz->Add(5, 0);
	sizer_mid_horiz->Add(sizer_mid_vert2, 0, 0, 0);
	sizer_mid_vert2->Add(informationGroupSizer, 0, wxTOP, 0);
	informationGroupSizer->Add(ResInfoGridSizer, 0, wxTOP, 3);
	ResInfoGridSizer->Add(ST_RunName, 0, wxALL, 3);
	ResInfoGridSizer->Add(TC_RunName, 0, wxALL, 3);
	ResInfoGridSizer->Add(ST_RunDate, 0, wxALL, 3);
	ResInfoGridSizer->Add(TC_RunDate, 0, wxALL, 3);
	ResInfoGridSizer->Add(ST_RunTime, 0, wxALL, 3);
	ResInfoGridSizer->Add(TC_RunTime, 0, wxALL, 3);
	ResInfoGridSizer->Add(ST_VersionNum, 0, wxALL, 3);
	ResInfoGridSizer->Add(TC_VersionNum, 0, wxALL, 3);
	ResInfoGridSizer->Add(ST_CoordinateSystem, 0, wxALL, 3);
	ResInfoGridSizer->Add(TC_CoordinateSystem, 0, wxALL, 3);
	ResInfoGridSizer->Add(ST_GasSpecies, 0, wxALL, 3);
	ResInfoGridSizer->Add(TC_GasSpecies, 0, wxALL, 3);
	ResInfoGridSizer->Add(ST_SolidPhases, 0, wxALL, 3);
	ResInfoGridSizer->Add(TC_SolidPhases, 0, wxALL, 3);
	ResInfoGridSizer->Add(ST_NumScalars, 0, wxALL, 3);
	ResInfoGridSizer->Add(TC_NumScalars, 0, wxALL, 3);
	ResInfoGridSizer->Add(ST_NumReactionRates, 0, wxALL, 3);
	ResInfoGridSizer->Add(TC_NumReactionRates, 0, wxALL, 3);
	ResInfoGridSizer->Add(ST_IMax, 0, wxALL, 3);
	ResInfoGridSizer->Add(TC_IMax, 0, wxALL, 3);
	ResInfoGridSizer->Add(ST_JMax, 0, wxALL, 3);
	ResInfoGridSizer->Add(TC_JMax, 0, wxALL, 3);
	ResInfoGridSizer->Add(ST_KMax, 0, wxALL, 3);
	ResInfoGridSizer->Add(TC_KMax, 0, wxALL, 3);

	// Options group
	sizer_mid_vert2->Add(0,25);
	sizer_mid_vert2->Add(optionsGroupSizer, 0, wxALL|wxEXPAND, 0);
	optionsGroupSizer->Add(exportGeometry, 0, wxALL, 3);
	optionsGroupSizer->Add(exportBoundingBox, 0, wxALL, 3);
	optionsGroupSizer->Add(static_line_options, 0, wxEXPAND, 0);
	optionsGroupSizer->Add(sizer_select_options, 0, wxALIGN_CENTER_HORIZONTAL, 3);
	sizer_select_options->Add(selectAllOptions, 0, wxALL|wxALIGN_CENTER_HORIZONTAL, 3);
        sizer_select_options->Add(10, 5, 0, 0);
        sizer_select_options->Add(unselectAllOptions, 0, wxALL|wxALIGN_CENTER_HORIZONTAL, 3);

	sizer_mid_horiz->Add(5, 0);
	sizer_mid_horiz->Add(static_line_8, 0, wxEXPAND|wxBOTTOM, 2);
	sizer_mid_horiz->Add(5, 0);

	sizer_mid_horiz->Add(sizer_12, 0, 0, 0);
	sizer_12->Add(outputVariablesSizer, 0, 0, 0);
	outputVariablesSizer->Add(VoidFractionVariable, 0, wxALL, 3);
	outputVariablesSizer->Add(GasPressureVariable, 0, wxALL, 3);
	outputVariablesSizer->Add(P_StarVariable, 0, wxALL, 3);
	outputVariablesSizer->Add(GasVelocityVariable, 0, wxALL, 3);
	outputVariablesSizer->Add(SolidPhaseVelocityVariable, 0, wxALL, 3);
	outputVariablesSizer->Add(SolidPhaseDensityVariable, 0, wxALL, 3);
	outputVariablesSizer->Add(TemperaturesVariable, 0, wxALL, 3);
	outputVariablesSizer->Add(GasSpeciesVariable, 0, wxALL, 3);
	outputVariablesSizer->Add(SolidPhaseSpeciesVariable, 0, wxALL, 3);
	outputVariablesSizer->Add(Theta_MVariable, 0, wxALL, 3);
	outputVariablesSizer->Add(ScalarsVariable, 0, wxALL, 3);
	outputVariablesSizer->Add(NumReactionRatesVariable, 0, wxALL, 3);
	outputVariablesSizer->Add(static_line_9, 0, wxALL|wxEXPAND, 3);

	outputVariablesSizer->Add(sizer_13, 0, wxALIGN_CENTER_HORIZONTAL, 3);
	sizer_13->Add(SelectAllOutputVariables, 0, wxALL|wxALIGN_CENTER_HORIZONTAL, 3);
	sizer_13->Add(10, 5, 0, 0);
	sizer_13->Add(UnselectAllOutputVariables, 0, wxALL|wxALIGN_CENTER_HORIZONTAL, 3);
	sizer_mid_horiz->Add(5, 0);

	sizer_12->Add(enableSpecificTSRangeSizer, 0, 0, 0);
	outputGridSizer->Add(BeginTSLabel, 0, wxALL, 3);
	outputGridSizer->Add(BeginTSBox, 0, wxALL, 3);
	outputGridSizer->Add(EndTSLabel, 0, wxALL, 3);
	outputGridSizer->Add(EndTSBox, 0, wxALL, 3);
	enableSpecificTSRangeSizer->Add(outputGridSizer, 0, 0, 0);
	enableSpecificTSRangeSizer->Add(static_line_10, 0, wxALL|wxEXPAND, 3);
	enableSpecificTSRangeSizer->Add(EnableSpecificTSRange, 0, wxALL, 3);
	sizer_12->Add(0, 5, 0, 0);

	sizer_7->Add(static_line_3, 0, wxALL|wxEXPAND, 0);
	sizer_7->Add(TotalProgress, 0, wxALL, 2);
	sizer_7->Add(ProgressGauge, 0, wxALL|wxALIGN_CENTER_HORIZONTAL|wxEXPAND, 4);
	sizer_7->Add(outputFormatText, 0, wxALL|wxALIGN_LEFT, 2);
	sizer_7->Add(static_line_2, 0, wxALL|wxEXPAND, 0);
	sizer_10->Add(20, 20, 0, 0, 0);
	
	sizer_10->Add(260, 0, 0, 0);
	sizer_10->Add(GoBttn, 0, wxALL|wxALIGN_CENTER_VERTICAL, 4);
	sizer_10->Add(20, 30, 0, 0, 0);
	sizer_10->Add(CancelBttn, 0, wxALL|wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL, 4);
	sizer_7->Add(sizer_10, 0, wxEXPAND, 0);
	SetAutoLayout(true);
	SetSizer(sizer_7);
	sizer_7->Fit(this);
	sizer_7->SetSizeHints(this);

	Layout();
}

// Event Table
BEGIN_EVENT_TABLE(mFixTranslator, wxDialog)
	EVT_BUTTON	(BUTTON_FILE_OPEN, mFixTranslator::OnFileOpenButton)
	EVT_BUTTON	(BUTTON_DEST, mFixTranslator::OnDestButton)
	EVT_BUTTON	(BUTTON_ABOUT, mFixTranslator::OnAboutButton)
	EVT_BUTTON	(BUTTON_GO, mFixTranslator::OnGoButton)
	EVT_BUTTON	(BUTTON_CANCEL, mFixTranslator::OnCancelButton)
	EVT_BUTTON	(wxID_CANCEL, mFixTranslator::OnCancelButton)
	EVT_CHECKBOX	(CHECKBOX_VTK, mFixTranslator::OnCheckboxLegacyVTK)
	EVT_RADIOBOX	(RADIOBOX_VTK, mFixTranslator::OnRadioBoxLegacyVTK)
	EVT_CHECKBOX	(CHECKBOX_SERIAL_XML, mFixTranslator::OnCheckboxSerialXML)
	EVT_CHECKBOX	(CHECKBOX_PARALLEL_XML, mFixTranslator::OnCheckboxParallelXML)
	EVT_CHECKBOX	(CHECKBOX_CGNS, mFixTranslator::OnCheckboxCGNS)
	EVT_BUTTON	(BUTTON_SELECTALL, mFixTranslator::OnButtonSelectAll)
	EVT_BUTTON	(BUTTON_UNSELECTALL, mFixTranslator::OnButtonUnselectAll)
	EVT_BUTTON	(BUTTON_SELECTALL_OPTIONS, mFixTranslator::OnButtonSelectAllOptions)
	EVT_BUTTON	(BUTTON_UNSELECTALL_OPTIONS, mFixTranslator::OnButtonUnselectAllOptions)
	EVT_BUTTON	(BUTTON_SELECTALLOUTPUTVARS, mFixTranslator::OnSelectAllOutputVariablesButton)
	EVT_BUTTON	(BUTTON_UNSELECTALLOUTPUTVARS, mFixTranslator::OnUnselectAllOutputVariablesButton)
	EVT_TEXT	(TEXTCTRL_BEGINNINGTS, mFixTranslator::OnTextCtrlBeginningTS)
	EVT_TEXT	(TEXTCTRL_ENDINGTS, mFixTranslator::OnTextCtrlEndingTS)
	EVT_CHECKBOX	(CHECKBOX_ENABLESPECIFICTSRANGE, mFixTranslator::OnCheckboxEnableSpecificTSRange)
	EVT_CHECKBOX	(CHECKBOX_VOIDFRACTION, mFixTranslator::UpdateMaxTS)
	EVT_CHECKBOX	(CHECKBOX_GASPRESSURE, mFixTranslator::UpdateMaxTS)
	EVT_CHECKBOX	(CHECKBOX_P_STAR, mFixTranslator::UpdateMaxTS)
	EVT_CHECKBOX	(CHECKBOX_GASVELOCITY, mFixTranslator::UpdateMaxTS)
	EVT_CHECKBOX	(CHECKBOX_SOLIDPHASEVELOCITY, mFixTranslator::UpdateMaxTS)
	EVT_CHECKBOX	(CHECKBOX_SOLIDPHASEDENSITY, mFixTranslator::UpdateMaxTS)
	EVT_CHECKBOX	(CHECKBOX_TEMPERATURE, mFixTranslator::UpdateMaxTS)
	EVT_CHECKBOX	(CHECKBOX_GASSPECIES, mFixTranslator::UpdateMaxTS)
	EVT_CHECKBOX	(CHECKBOX_SOLIDPHASESPECIES, mFixTranslator::UpdateMaxTS)
	EVT_CHECKBOX	(CHECKBOX_THETA_M, mFixTranslator::UpdateMaxTS)
	EVT_CHECKBOX	(CHECKBOX_SCALARS, mFixTranslator::UpdateMaxTS)
	EVT_CHECKBOX	(CHECKBOX_NUMREACTIONRATES, mFixTranslator::UpdateMaxTS)
END_EVENT_TABLE()
	
// Added functions
void mFixTranslator::FormatDate(int iDay, int iMonth, int iYear)
{
	wxString strMonth;

	switch(iMonth)
	{
	case 1:
		strMonth << wxT("January");
		break;
	case 2:
		strMonth << wxT("February");
		break;
	case 3:
		strMonth << wxT("March");
		break;
	case 4:
		strMonth << wxT("April");
		break;
	case 5:
		strMonth << wxT("May");
		break;
	case 6:
		strMonth << wxT("June");
		break;
	case 7:
		strMonth << wxT("July");
		break;
	case 8:
		strMonth << wxT("August");
		break;
	case 9:
		strMonth << wxT("September");
		break;
	case 10:
		strMonth << wxT("October");
		break;
	case 11:
		strMonth << wxT("November");
		break;
	case 12:
		strMonth << wxT("December");
		break;
	default:
		strMonth << iMonth;
	}

	(*TC_RunDate) << strMonth;
	(*TC_RunDate) << wxT(" ");
	(*TC_RunDate) << iDay;
	(*TC_RunDate) << wxT(", ");
	(*TC_RunDate) << iYear;
	
	wxString statusReportDate;
	statusReportDate.Printf(wxT("Run Date: %s %i, %i\n"), strMonth, iDay, iYear);
	text_ctrl_statusReport->AppendText(statusReportDate);
	TC_RunDate->SetInsertionPoint(0);
}

void mFixTranslator::FormatTime(int iSecond, int iMinute, int iHour)
{
	(*TC_RunTime) << iHour;
	(*TC_RunTime) << wxT(":");
	(*TC_RunTime) << iMinute;
	(*TC_RunTime) << wxT(":");
	(*TC_RunTime) << iSecond;

	wxString statusReportTime;
	statusReportTime.Printf(wxT("Run Time: %i:%i:%i\n"), iHour, iMinute, iSecond);
	text_ctrl_statusReport->AppendText(statusReportTime);
	TC_RunTime->SetInsertionPoint(0);
}

void mFixTranslator::FlushGUI(void)
{
	static_text_1_TS->Enable(TRUE);
	static_text_1_TS->SetLabel(wxT("0/0 Timesteps"));
	static_text_1->Enable(TRUE);
	SPGauge_1->SetValue(0);
	SPGauge_1->Enable(TRUE);
	SPGauge_1->SetBackgroundColour(wxColour(0, 255, 0));

	static_text_2_TS->Enable(TRUE);
	static_text_2_TS->SetLabel(wxT("0/0 Timesteps"));
	static_text_2->Enable(TRUE);
	SPGauge_2->SetValue(0);
	SPGauge_2->Enable(TRUE);
	SPGauge_2->SetBackgroundColour(wxColour(0, 255, 0));

	static_text_3_TS->Enable(TRUE);
	static_text_3_TS->SetLabel(wxT("0/0 Timesteps"));
	static_text_3->Enable(TRUE);
	SPGauge_3->SetValue(0);
	SPGauge_3->Enable(TRUE);
	SPGauge_3->SetBackgroundColour(wxColour(0, 255, 0));

	static_text_4_TS->Enable(TRUE);
	static_text_4_TS->SetLabel(wxT("0/0 Timesteps"));
	static_text_4->Enable(TRUE);
	SPGauge_4->SetValue(0);
	SPGauge_4->Enable(TRUE);
	SPGauge_4->SetBackgroundColour(wxColour(0, 255, 0));

	static_text_5_TS->Enable(TRUE);
	static_text_5_TS->SetLabel(wxT("0/0 Timesteps"));
	static_text_5->Enable(TRUE);
	SPGauge_5->SetValue(0);
	SPGauge_5->Enable(TRUE);
	SPGauge_5->SetBackgroundColour(wxColour(0, 255, 0));

	static_text_6_TS->Enable(TRUE);
	static_text_6_TS->SetLabel(wxT("0/0 Timesteps"));
	static_text_6->Enable(TRUE);
	SPGauge_6->SetValue(0);
	SPGauge_6->Enable(TRUE);
	SPGauge_6->SetBackgroundColour(wxColour(0, 255, 0));

	static_text_7_TS->Enable(TRUE);
	static_text_7_TS->SetLabel(wxT("0/0 Timesteps"));
	static_text_7->Enable(TRUE);
	SPGauge_7->SetValue(0);
	SPGauge_7->Enable(TRUE);
	SPGauge_7->SetBackgroundColour(wxColour(0, 255, 0));

	static_text_8_TS->Enable(TRUE);
	static_text_8_TS->SetLabel(wxT("0/0 Timesteps"));
	static_text_8->Enable(TRUE);
	SPGauge_8->SetValue(0);
	SPGauge_8->Enable(TRUE);
	SPGauge_8->SetBackgroundColour(wxColour(0, 255, 0));

	static_text_9_TS->Enable(TRUE);
	static_text_9_TS->SetLabel(wxT("0/0 Timesteps"));
	static_text_9->Enable(TRUE);
	SPGauge_9->SetValue(0);
	SPGauge_9->Enable(TRUE);
	SPGauge_9->SetBackgroundColour(wxColour(0, 255, 0));

	static_text_a_TS->Enable(TRUE);
	static_text_a_TS->SetLabel(wxT("0/0 Timesteps"));
	static_text_a->Enable(TRUE);
	SPGauge_a->SetValue(0);
	SPGauge_a->Enable(TRUE);
	SPGauge_a->SetBackgroundColour(wxColour(0, 255, 0));

	TC_RunName->Clear();
	TC_RunDate->Clear();
	TC_RunTime->Clear();
	TC_VersionNum->Clear();
	TC_CoordinateSystem->Clear();
	TC_GasSpecies->Clear();
	TC_SolidPhases->Clear();
	TC_NumScalars->Clear();
	TC_NumReactionRates->Clear();
	TC_IMax->Clear();
	TC_JMax->Clear();
	TC_KMax->Clear();

	legacyVTKCheckbox->Enable(TRUE);
	legacyVTKCheckbox->SetValue(TRUE);
	legacyVTKRadioBox->Enable(TRUE);
	legacyVTKRadioBox->SetSelection(1);
	serialXMLCheckbox->Enable(TRUE);
	serialXMLCheckbox->SetValue(FALSE);
	parallelXMLCheckbox->Enable(TRUE);
	parallelXMLCheckbox->SetValue(FALSE);
	cgnsCheckbox->Enable(TRUE);
	cgnsCheckbox->SetValue(FALSE);
	selectAllButton->Enable(TRUE);
	unselectAllButton->Enable(TRUE);

	exportGeometry->Enable(TRUE);
	exportGeometry->SetValue(FALSE);
	exportBoundingBox->Enable(TRUE);
	exportBoundingBox->SetValue(FALSE);
	selectAllOptions->Enable(TRUE);
	unselectAllOptions->Enable(TRUE);

	VoidFractionVariable->SetValue(FALSE);
	GasPressureVariable->SetValue(FALSE);
	P_StarVariable->SetValue(FALSE);
	GasVelocityVariable->SetValue(FALSE);
	SolidPhaseVelocityVariable->SetValue(FALSE);
	SolidPhaseDensityVariable->SetValue(FALSE);
	TemperaturesVariable->SetValue(FALSE);
	GasSpeciesVariable->SetValue(FALSE);
	SolidPhaseSpeciesVariable->SetValue(FALSE);
	Theta_MVariable->SetValue(FALSE);
	ScalarsVariable->SetValue(FALSE);
	NumReactionRatesVariable->SetValue(FALSE);
	SelectAllOutputVariables->Enable(TRUE);
	UnselectAllOutputVariables->Enable(TRUE);

	EnableSpecificTSRange->Enable(TRUE);
	EnableSpecificTSRange->SetValue(FALSE);

	text_ctrl_statusReport->Clear();
	enableLoggingCheckbox->Enable(TRUE);
	enableLoggingCheckbox->SetValue(FALSE);

	ProgressGauge->SetValue(0);
	outputFormatText->SetLabel(wxT(""));

	sp1Active = FALSE;
        sp2Active = FALSE;
        sp3Active = FALSE;
        sp4Active = FALSE;
        sp5Active = FALSE;
        sp6Active = FALSE;
        sp7Active = FALSE;
        sp8Active = FALSE;
        sp9Active = FALSE;
        spaActive = FALSE;

	GoBttn->Enable(TRUE);

	formatTally = 0;
	formatCurrent = 0;
	totalTimeStepTally = 0;

	OutputDirectory = wxGetCwd();
	memcpy(Dir, OutputDirectory.c_str(), (OutputDirectory.length()+1));

	wxString OutDirStatus;
	OutDirStatus.Printf(wxT("output directory: %s\n"), Dir);
	text_ctrl_statusReport->AppendText(OutDirStatus);
}

int mFixTranslator::DetermineMaxTimeSteps(void)
{
	int maxTimeSteps = 0;

	if (VoidFractionVariable->GetValue() == TRUE) {
		if(sp1TimeSteps > maxTimeSteps)
			maxTimeSteps = sp1TimeSteps;
	}

	if (GasPressureVariable->GetValue() == TRUE || P_StarVariable->GetValue() == TRUE) {
		if(sp2TimeSteps > maxTimeSteps)
			maxTimeSteps = sp2TimeSteps;
	}

	if (GasVelocityVariable->GetValue() == TRUE) {
		if(sp3TimeSteps > maxTimeSteps)
			maxTimeSteps = sp3TimeSteps;
	}

	if (SolidPhaseVelocityVariable->GetValue() == TRUE) {
		if(sp4TimeSteps > maxTimeSteps)
			maxTimeSteps = sp4TimeSteps;
	}

	if (SolidPhaseDensityVariable->GetValue() == TRUE) {
		if(sp5TimeSteps > maxTimeSteps)
			maxTimeSteps = sp5TimeSteps;
	}

	if (TemperaturesVariable->GetValue() == TRUE) {
		if(sp6TimeSteps > maxTimeSteps)
			maxTimeSteps = sp6TimeSteps;
	}

	if (GasSpeciesVariable->GetValue() == TRUE || SolidPhaseSpeciesVariable->GetValue() == TRUE) {
		if(sp7TimeSteps > maxTimeSteps)
			maxTimeSteps = sp7TimeSteps;
	}

	if (Theta_MVariable->GetValue() == TRUE) {
		if(sp8TimeSteps > maxTimeSteps)
			maxTimeSteps = sp8TimeSteps;
	}

	if (ScalarsVariable->GetValue() == TRUE) {
		if(sp9TimeSteps > maxTimeSteps)
			maxTimeSteps = sp9TimeSteps;
	}

	if (NumReactionRatesVariable->GetValue() == TRUE) {
		if(spaTimeSteps > maxTimeSteps)
			maxTimeSteps = spaTimeSteps;
	}

	return maxTimeSteps;
}


void mFixTranslator::SetOutputVariableFlags(void)
{
	if(VoidFractionVariable->GetValue() == TRUE)
	{	
		resH.varSelectFlag[0] = 1;
	} else {
		resH.varSelectFlag[0] = 0;
	}
	if(GasPressureVariable->GetValue() == TRUE)
	{	
		resH.varSelectFlag[1] = 1;
	} else {
		resH.varSelectFlag[1] = 0;
	}
	if(P_StarVariable->GetValue() == TRUE)
	{	
		resH.varSelectFlag[2] = 1;
	} else {
		resH.varSelectFlag[2] = 0;
	}
	if(GasVelocityVariable->GetValue() == TRUE)
	{	
		resH.varSelectFlag[3] = 1;
	} else {
		resH.varSelectFlag[3] = 0;
	}
	if(SolidPhaseVelocityVariable->GetValue() == TRUE)
	{	
		resH.varSelectFlag[4] = 1;
	} else {
		resH.varSelectFlag[4] = 0;
	}
	if(SolidPhaseDensityVariable->GetValue() == TRUE)
	{	
		resH.varSelectFlag[5] = 1;
	} else {
		resH.varSelectFlag[5] = 0;
	}
	if(TemperaturesVariable->GetValue() == TRUE)
	{	
		resH.varSelectFlag[6] = 1;
	} else {
		resH.varSelectFlag[6] = 0;
	}
	if(GasSpeciesVariable->GetValue() == TRUE)
	{	
		resH.varSelectFlag[7] = 1;
	} else {
		resH.varSelectFlag[7] = 0;
	}
	if(SolidPhaseSpeciesVariable->GetValue() == TRUE)
	{	
		resH.varSelectFlag[8] = 1;
	} else {
		resH.varSelectFlag[8] = 0;
	}
	if(Theta_MVariable->GetValue() == TRUE)
	{	
		resH.varSelectFlag[9] = 1;
	} else {
		resH.varSelectFlag[9] = 0;
	}
	if(ScalarsVariable->GetValue() == TRUE)
	{	
		resH.varSelectFlag[10] = 1;
	} else {
		resH.varSelectFlag[10] = 0;
	}
	if(NumReactionRatesVariable->GetValue() == TRUE)
	{	
		resH.varSelectFlag[11] = 1;
	} else {
		resH.varSelectFlag[11] = 0;
	}
}

void mFixTranslator::OnDestButton(wxCommandEvent& event)
{		
	OutputDirectory = wxDirSelector("Choose a folder", wxGetCwd(), wxDD_NEW_DIR_BUTTON);
	if (OutputDirectory.empty())
		OutputDirectory = wxGetCwd();

	memcpy(Dir, OutputDirectory.c_str(), (OutputDirectory.length()+1));
	wxString OutDirStatus;
	OutDirStatus.Printf(wxT("output directory: %s\n"), Dir);
	text_ctrl_statusReport->AppendText(OutDirStatus);
}

void mFixTranslator::OnFileOpenButton(wxCommandEvent& event)
{		
	OpenFileDialog = new wxFileDialog(this, wxT("Choose a Restart File..."), wxT(""), wxT(""), wxT("*.res|*.RES"), wxOPEN|wxHIDE_READONLY, wxDefaultPosition);
	if(OpenFileDialog->ShowModal() == wxID_OK)
	{
		// Flush GUI
		FlushGUI();

		// Load MFIX data
		inFilename = new char[255];
		wxString OpenFilename;
		OpenFilename = OpenFileDialog->GetPath();
		memcpy(inFilename, OpenFilename.c_str(), (OpenFilename.length()+1));
		 ReadResFile(inFilename, &resH);

		//Populate Restart File Information TextControls
		resH.runName[20] = 0;	// max length of 60
		wxCSConv conv_ascii(_T("ISO-8859-1"));
		wxString *wxTemp;
		wxTemp = new wxString(resH.runName, conv_ascii);
		TC_RunName->WriteText(*wxTemp);
		TC_RunName->SetInsertionPoint(0);
		text_ctrl_statusReport->AppendText(wxT("Run Name:  "));
		text_ctrl_statusReport->AppendText(*wxTemp);
		text_ctrl_statusReport->AppendText(wxT("\n"));
		delete wxTemp;
		
		FormatDate(resH.idDay, resH.idMonth, resH.idYear);
		FormatTime(resH.idSecond, resH.idMinute, resH.idHour);

		resH.version[12] = 0;	// max length of 12
		wxTemp = new wxString(resH.version, conv_ascii);
		TC_VersionNum->WriteText(*wxTemp);
		TC_VersionNum->SetInsertionPoint(0);
		text_ctrl_statusReport->AppendText(wxT("Version Number:  "));
		text_ctrl_statusReport->AppendText(*wxTemp);
		text_ctrl_statusReport->AppendText(wxT("\n"));
		delete wxTemp;
	
		resH.coordinates[12] = 0;	// max length of 17
		wxTemp = new wxString(resH.coordinates, conv_ascii);
		TC_CoordinateSystem->WriteText(*wxTemp);
		TC_CoordinateSystem->SetInsertionPoint(0);
		text_ctrl_statusReport->AppendText(wxT("Coordinate System:  "));		
		text_ctrl_statusReport->AppendText(*wxTemp);
		text_ctrl_statusReport->AppendText(wxT("\n"));
		delete wxTemp;

		(*TC_GasSpecies) << resH.nMax[0];	// Still a little unclear on this one
		TC_GasSpecies->SetInsertionPoint(0);
		wxString gasSpecies;
		gasSpecies.Printf(wxT("Gas Species:  %i\n"), resH.nMax[0]);
		text_ctrl_statusReport->AppendText(gasSpecies);
		(*TC_SolidPhases) << resH.mMax;
		TC_SolidPhases->SetInsertionPoint(0);
		wxString solidPhases;
		solidPhases.Printf(wxT("Solid Phases: %i\n"), resH.mMax);
		text_ctrl_statusReport->AppendText(solidPhases);
		(*TC_NumScalars) << resH.nscalar;
		TC_NumScalars->SetInsertionPoint(0);
		wxString numScalars;
		numScalars.Printf(wxT("Number of Scalars: %i\n"), resH.nscalar);
		text_ctrl_statusReport->AppendText(numScalars);
		(*TC_NumReactionRates) << resH.nrr;
		TC_NumReactionRates->SetInsertionPoint(0);
		wxString numReactionRates;
		numReactionRates.Printf(wxT("Number of Reaction Rates: %i\n"), resH.nrr);
		text_ctrl_statusReport->AppendText(numReactionRates);
		(*TC_IMax) << resH.iMax;
		TC_IMax->SetInsertionPoint(0);
		wxString iMax_str;
		iMax_str.Printf(wxT("iMax: %i\n"), resH.iMax);
		text_ctrl_statusReport->AppendText(iMax_str);
		(*TC_JMax) << resH.jMax;
		TC_JMax->SetInsertionPoint(0);
		wxString jMax_str;
		jMax_str.Printf(wxT("jMax: %i\n"), resH.jMax);
		text_ctrl_statusReport->AppendText(jMax_str);
		(*TC_KMax) << resH.kMax;
		TC_KMax->SetInsertionPoint(0);
		wxString kMax_str;
		kMax_str.Printf(wxT("kMax: %i\n"), resH.kMax);
		text_ctrl_statusReport->AppendText(kMax_str);

		text_ctrl_statusReport->AppendText(wxT("\n"));

		// Strip ".res" off of file to search for other extensions
		OpenFilename = OpenFilename.BeforeLast(wxT('.'));

		// Check for existence of all files
		// Check for sp1
		if(wxFileExists(OpenFilename.Append(wxT(".SP1"))))
		{
			SPGauge_1->SetBackgroundColour(wxColour(0,255,0));
			wxString sp1TimeStepsNum = wxT("0/"); 
			memcpy(inFilename, OpenFilename.c_str(), (OpenFilename.length()+1));
			sp1TimeSteps = ReadSPTimesteps(inFilename);
			if (sp1TimeSteps > 0)
				SPGauge_1->SetRange(sp1TimeSteps);
			else
				SPGauge_1->SetRange(sp1TimeSteps+1);
			wxString sp1StatusOutput;
			sp1StatusOutput.Printf(wxT("sp1 file is present with %i timesteps\n"), sp1TimeSteps);
			text_ctrl_statusReport->AppendText(sp1StatusOutput);
			VoidFractionVariable->Enable(TRUE);
			sp1Exists = true;
		} else {
			SPGauge_1->SetBackgroundColour(wxColour(255,0,0));
			static_text_1_TS->SetLabel(wxT("0/0 Timesteps"));
			SPGauge_1->SetToolTip(wxT("No sp1 file available"));
			sp1TimeSteps = 0;
			sp1Exists = false;
		}
		// Check for sp2
		OpenFilename = OpenFilename.BeforeLast(wxT('.'));
		if(wxFileExists(OpenFilename.Append(wxT(".SP2"))))
		{
			SPGauge_2->SetBackgroundColour(wxColour(0,255,0));
			wxString sp2TimeStepsNum = wxT("0/"); 
			memcpy(inFilename, OpenFilename.c_str(), (OpenFilename.length()+1));
			sp2TimeSteps = ReadSPTimesteps(inFilename);
			if (sp2TimeSteps > 0)
				SPGauge_2->SetRange(sp2TimeSteps);
			else
				SPGauge_2->SetRange(sp2TimeSteps+1);
			wxString sp2StatusOutput;
			sp2StatusOutput.Printf(wxT("sp2 file is present with %i timesteps\n"), sp2TimeSteps);
			text_ctrl_statusReport->AppendText(sp2StatusOutput);
			GasPressureVariable->Enable(TRUE);
			P_StarVariable->Enable(TRUE);
			sp2Exists = true;
		} else {
			SPGauge_2->SetBackgroundColour(wxColour(255,0,0));
			static_text_2_TS->SetLabel(wxT("0/0 Timesteps"));
			SPGauge_2->SetToolTip(wxT("No sp2 file available"));
			sp2TimeSteps = 0;
			sp2Exists = false;
		}
		// Check for sp3
		OpenFilename = OpenFilename.BeforeLast(wxT('.'));
		if(wxFileExists(OpenFilename.Append(wxT(".SP3"))))
		{
			SPGauge_3->SetBackgroundColour(wxColour(0,255,0));
			wxString sp3TimeStepsNum = wxT("0/"); 
			memcpy(inFilename, OpenFilename.c_str(), (OpenFilename.length()+1));
			sp3TimeSteps = ReadSPTimesteps(inFilename);
			if (sp3TimeSteps > 0)
				SPGauge_3->SetRange(sp3TimeSteps);
			else
				SPGauge_3->SetRange(sp3TimeSteps+1);
			wxString sp3StatusOutput;
			sp3StatusOutput.Printf(wxT("sp3 file is present with %i timesteps\n"), sp3TimeSteps);
			text_ctrl_statusReport->AppendText(sp3StatusOutput);
			GasVelocityVariable->Enable(TRUE);
			sp3Exists = true;
		} else {
			SPGauge_3->SetBackgroundColour(wxColour(255,0,0));
			static_text_3_TS->SetLabel(wxT("0/0 Timesteps"));
			SPGauge_3->SetToolTip(wxT("No sp3 file available"));
			sp3TimeSteps = 0;
			sp3Exists = false;
		}
		// Check for sp4
		OpenFilename = OpenFilename.BeforeLast(wxT('.'));
		if(wxFileExists(OpenFilename.Append(wxT(".SP4"))))
		{
			SPGauge_4->SetBackgroundColour(wxColour(0,255,0));
			wxString sp4TimeStepsNum = wxT("0/"); 
			memcpy(inFilename, OpenFilename.c_str(), (OpenFilename.length()+1));
			sp4TimeSteps = ReadSPTimesteps(inFilename);
			if (sp4TimeSteps > 0)
				SPGauge_4->SetRange(sp4TimeSteps);
			else
				SPGauge_4->SetRange(sp4TimeSteps+1);
			wxString sp4StatusOutput;
			sp4StatusOutput.Printf(wxT("sp4 file is present with %i timesteps\n"), sp4TimeSteps);
			text_ctrl_statusReport->AppendText(sp4StatusOutput);
			SolidPhaseVelocityVariable->Enable(TRUE);
			sp4Exists = true;
		} else {
			SPGauge_4->SetBackgroundColour(wxColour(255,0,0));
			static_text_4_TS->SetLabel(wxT("0/0 Timesteps"));
			SPGauge_4->SetToolTip(wxT("No sp4 file available"));
			sp4TimeSteps = 0;
			sp4Exists = false;
		}
		// Check for sp5
		OpenFilename = OpenFilename.BeforeLast(wxT('.'));
		if(wxFileExists(OpenFilename.Append(wxT(".SP5"))))
		{
			SPGauge_5->SetBackgroundColour(wxColour(0,255,0));
			wxString sp5TimeStepsNum = wxT("0/"); 
			memcpy(inFilename, OpenFilename.c_str(), (OpenFilename.length()+1));
			sp5TimeSteps = ReadSPTimesteps(inFilename);
			if (sp5TimeSteps > 0)
				SPGauge_5->SetRange(sp5TimeSteps);
			else
				SPGauge_5->SetRange(sp5TimeSteps+1);
			wxString sp5StatusOutput;
			sp5StatusOutput.Printf(wxT("sp5 file is present with %i timesteps\n"), sp5TimeSteps);
			text_ctrl_statusReport->AppendText(sp5StatusOutput);
			SolidPhaseDensityVariable->Enable(TRUE);
			sp5Exists = true;
		} else {
			SPGauge_5->SetBackgroundColour(wxColour(255,0,0));
			static_text_5_TS->SetLabel(wxT("0/0 Timesteps"));
			SPGauge_5->SetToolTip(wxT("No sp5 file available"));
			sp5TimeSteps = 0;
			sp5Exists = false;
		}
		// Check for sp6
		OpenFilename = OpenFilename.BeforeLast(wxT('.'));
		if(wxFileExists(OpenFilename.Append(wxT(".SP6"))))
		{
			SPGauge_6->SetBackgroundColour(wxColour(0,255,0));
			wxString sp6TimeStepsNum = wxT("0/"); 
			memcpy(inFilename, OpenFilename.c_str(), (OpenFilename.length()+1));
			sp6TimeSteps = ReadSPTimesteps(inFilename);
			if (sp6TimeSteps > 0)
				SPGauge_6->SetRange(sp6TimeSteps);
			else
				SPGauge_6->SetRange(sp6TimeSteps+1);
			wxString sp6StatusOutput;
			sp6StatusOutput.Printf(wxT("sp6 file is present with %i timesteps\n"), sp6TimeSteps);
			text_ctrl_statusReport->AppendText(sp6StatusOutput);
			TemperaturesVariable->Enable(TRUE);
			sp6Exists = true;
		} else {
			SPGauge_6->SetBackgroundColour(wxColour(255,0,0));
			static_text_6_TS->SetLabel(wxT("0/0 Timesteps"));
			SPGauge_6->SetToolTip(wxT("No sp6 file available"));
			sp6TimeSteps = 0;
			sp6Exists = false;
		}
		// Check for sp7
		OpenFilename = OpenFilename.BeforeLast(wxT('.'));
		if(wxFileExists(OpenFilename.Append(wxT(".SP7"))))
		{
			SPGauge_7->SetBackgroundColour(wxColour(0,255,0));
			wxString sp7TimeStepsNum = wxT("0/"); 
			memcpy(inFilename, OpenFilename.c_str(), (OpenFilename.length()+1));
			sp7TimeSteps = ReadSPTimesteps(inFilename);
			if (sp7TimeSteps > 0)
				SPGauge_7->SetRange(sp7TimeSteps);
			else
				SPGauge_7->SetRange(sp7TimeSteps+1);
			wxString sp7StatusOutput;
			sp7StatusOutput.Printf(wxT("sp7 file is present with %i timesteps\n"), sp7TimeSteps);
			text_ctrl_statusReport->AppendText(sp7StatusOutput);
			GasSpeciesVariable->Enable(TRUE);
			SolidPhaseSpeciesVariable->Enable(TRUE);
			sp7Exists = true;
		} else {
			SPGauge_7->SetBackgroundColour(wxColour(255,0,0));
			static_text_7_TS->SetLabel(wxT("0/0 Timesteps"));
			SPGauge_7->SetToolTip(wxT("No sp7 file available"));
			sp7TimeSteps = 0;
			sp7Exists = false;
		}
		// Check for sp8
		OpenFilename = OpenFilename.BeforeLast(wxT('.'));
		if(wxFileExists(OpenFilename.Append(wxT(".SP8"))))
		{
			SPGauge_8->SetBackgroundColour(wxColour(0,255,0));
			wxString sp8TimeStepsNum = wxT("0/");
			memcpy(inFilename, OpenFilename.c_str(), (OpenFilename.length()+1));
			sp8TimeSteps = ReadSPTimesteps(inFilename);
			if (sp8TimeSteps > 0)
				SPGauge_8->SetRange(sp8TimeSteps);
			else
				SPGauge_8->SetRange(sp8TimeSteps+1);
			wxString sp8StatusOutput;
			sp8StatusOutput.Printf(wxT("sp8 file is present with %i timesteps\n"), sp8TimeSteps);
			text_ctrl_statusReport->AppendText(sp8StatusOutput);
			Theta_MVariable->Enable(TRUE);
			sp8Exists = true;
		} else {
			SPGauge_8->SetBackgroundColour(wxColour(255,0,0));
			static_text_8_TS->SetLabel(wxT("0/0 Timesteps"));
			SPGauge_8->SetToolTip(wxT("No sp8 file available"));
			sp8TimeSteps = 0;
			sp8Exists = false;
		}
		// Check for sp9
		OpenFilename = OpenFilename.BeforeLast(wxT('.'));
		if(wxFileExists(OpenFilename.Append(wxT(".SP9"))))
		{
			SPGauge_9->SetBackgroundColour(wxColour(0,255,0));
			wxString sp9TimeStepsNum = wxT("0/"); 
			memcpy(inFilename, OpenFilename.c_str(), (OpenFilename.length()+1));
			sp9TimeSteps = ReadSPTimesteps(inFilename);
			if (sp9TimeSteps > 0)
				SPGauge_9->SetRange(sp9TimeSteps);
			else
				SPGauge_9->SetRange(sp9TimeSteps+1);
			wxString sp9StatusOutput;
			sp9StatusOutput.Printf(wxT("sp9 file is present with %i timesteps\n"), sp9TimeSteps);
			text_ctrl_statusReport->AppendText(sp9StatusOutput);
			ScalarsVariable->Enable(TRUE);
			sp9Exists = true;
		} else {
			SPGauge_9->SetBackgroundColour(wxColour(255,0,0));
			static_text_9_TS->SetLabel(wxT("0/0 Timesteps"));
			SPGauge_9->SetToolTip(wxT("No sp9 file available"));
			sp9TimeSteps = 0;
			sp9Exists = false;
		}

		// Check for spa
		OpenFilename = OpenFilename.BeforeLast(wxT('.'));
		if(wxFileExists(OpenFilename.Append(wxT(".SPA"))))
		{
			SPGauge_a->SetBackgroundColour(wxColour(0,255,0));
			wxString spaTimeStepsNum = wxT("0/"); 
			memcpy(inFilename, OpenFilename.c_str(), (OpenFilename.length()+1));
			spaTimeSteps = ReadSPTimesteps(inFilename);
			if (spaTimeSteps > 0)
				SPGauge_a->SetRange(spaTimeSteps);
			else
				SPGauge_a->SetRange(spaTimeSteps+1);
			wxString spaStatusOutput;
			spaStatusOutput.Printf(wxT("spa file is present with %i timesteps\n"), spaTimeSteps);
			text_ctrl_statusReport->AppendText(spaStatusOutput);
			NumReactionRatesVariable->Enable(TRUE);
			spaExists = true;
		} else {
			SPGauge_a->SetBackgroundColour(wxColour(255,0,0));
			static_text_a_TS->SetLabel(wxT("0/0 Timesteps"));
			SPGauge_a->SetToolTip(wxT("No spa file available"));
			spaTimeSteps = 0;
			spaExists = false;
		}
		wxString sp1TimeStepsNum = wxT("0/");
		sp1TimeStepsNum << sp1TimeSteps << wxT(" Timesteps");
		static_text_1_TS->SetLabel(sp1TimeStepsNum);
		wxString sp2TimeStepsNum = wxT("0/");
		sp2TimeStepsNum << sp2TimeSteps << wxT(" Timesteps");
		static_text_2_TS->SetLabel(sp2TimeStepsNum);
		wxString sp3TimeStepsNum = wxT("0/");
		sp3TimeStepsNum << sp3TimeSteps << wxT(" Timesteps");
		static_text_3_TS->SetLabel(sp3TimeStepsNum);
		wxString sp4TimeStepsNum = wxT("0/");
		sp4TimeStepsNum << sp4TimeSteps << wxT(" Timesteps");
		static_text_4_TS->SetLabel(sp4TimeStepsNum);
		wxString sp5TimeStepsNum = wxT("0/");
		sp5TimeStepsNum << sp5TimeSteps << wxT(" Timesteps");
		static_text_5_TS->SetLabel(sp5TimeStepsNum);
		wxString sp6TimeStepsNum = wxT("0/");
		sp6TimeStepsNum << sp6TimeSteps << wxT(" Timesteps");
		static_text_6_TS->SetLabel(sp6TimeStepsNum);
		wxString sp7TimeStepsNum = wxT("0/");
		sp7TimeStepsNum << sp7TimeSteps << wxT(" Timesteps");
		static_text_7_TS->SetLabel(sp7TimeStepsNum);
		wxString sp8TimeStepsNum = wxT("0/");
		sp8TimeStepsNum << sp8TimeSteps << wxT(" Timesteps");
		static_text_8_TS->SetLabel(sp8TimeStepsNum);
		wxString sp9TimeStepsNum = wxT("0/");
		sp9TimeStepsNum << sp9TimeSteps << wxT(" Timesteps");
		static_text_9_TS->SetLabel(sp9TimeStepsNum);
		wxString spaTimeStepsNum = wxT("0/");
		spaTimeStepsNum << spaTimeSteps << wxT(" Timesteps");
		static_text_a_TS->SetLabel(spaTimeStepsNum);
	}
	OpenFileDialog->Destroy();
	GoBttn->Enable(TRUE);
}

void mFixTranslator::OnAboutButton(wxCommandEvent& event)
{		
	wxMessageBox(wxT("MFIX to VTK Data Translator\nWVVEL - 2004, v1.0.3\n------------------------------------------\nJim Canon <jcanon@csee.wvu.edu>\nTrubie Turner <flexei@hotmail.com\nJeremy Jarrell <jarrell@csee.wvu.edu>"), wxT("About"), wxOK); }

void mFixTranslator::OnGoButton(wxCommandEvent& event)
{
	wxString wxInFile;
	wxString FileOutputDisplay;
	formatTally = 0;
	formatCurrent = 0;
	
	// Strip off suffix to append 
	string ProjectName;
	ProjectName = inFilename;
	ProjectName.erase(ProjectName.find_last_of("."), (ProjectName.length()+1));

	int MaxTimeSteps = DetermineMaxTimeSteps();

	// Update the log file
	wxCSConv conv_ascii(_T("ISO-8859-1"));
	wxString logFile(inFilename, conv_ascii);

	// Strip off sp file name to append log.txt to output path
	//while(logFile.Last() != '\\')
	//{
	//	logFile.RemoveLast();
	//}
	logFile.Append(wxT("log.txt"));

	if(enableLoggingCheckbox->IsChecked())
	{
		text_ctrl_statusReport->SaveFile(logFile);
	}

	// Tally the total different formats we'll be writing
	if(legacyVTKCheckbox->IsChecked()){
		formatTally++;
		if(legacyVTKRadioBox->GetSelection()==0)
		{
			text_ctrl_statusReport->AppendText(wxT("Legacy VTK (ASCII) has been selected for output.\n"));
		} else {
			text_ctrl_statusReport->AppendText(wxT("Legacy VTK (Binary) has been selected for output.\n"));
		}
	}
	if(serialXMLCheckbox->IsChecked()){
		formatTally++;
		text_ctrl_statusReport->AppendText(wxT("Serial XML has been selected for output.\n"));
	}
	if(parallelXMLCheckbox->IsChecked()){
		formatTally++;
		text_ctrl_statusReport->AppendText(wxT("Parallel XML has been selected for output.\n"));
	}
	if(cgnsCheckbox->IsChecked()){
		formatTally++;
		text_ctrl_statusReport->AppendText(wxT("CGNS has been selected for output.\n"));
	}

	// Determine if specific timestep ranges have been selected and if so set accordingly
	if(EnableSpecificTSRange->GetValue() == TRUE)
	{
		wxString wxTemp;
		wxTemp = BeginTSBox->GetValue();
		BeginTSVal =atoi((const char *)wxTemp.c_str());
		wxTemp = EndTSBox->GetValue();
		EndTSVal = atoi((const char *)wxTemp.c_str());
	} else {
		BeginTSVal = 0;
		EndTSVal = DetermineMaxTimeSteps();
	}

	// Check for at least one ouput format selected
	if((!legacyVTKCheckbox->IsChecked()) && (!serialXMLCheckbox->IsChecked()) && (!parallelXMLCheckbox->IsChecked()) 
			&&  (!cgnsCheckbox->IsChecked()))
	{
		wxMessageBox(wxT("Please select at least one output format!"));
		wxBell();
		// Check for at least one output variable selected
	}	else if((!VoidFractionVariable->IsChecked()) && (!GasPressureVariable->IsChecked()) && (!P_StarVariable->IsChecked()) 
			&& (!GasVelocityVariable->IsChecked()) && (!SolidPhaseVelocityVariable->IsChecked()) && (!SolidPhaseDensityVariable->IsChecked())
			&& (!TemperaturesVariable->IsChecked()) && (!GasSpeciesVariable->IsChecked()) && (!SolidPhaseSpeciesVariable->IsChecked())
			&& (!Theta_MVariable->IsChecked()) && (!ScalarsVariable->IsChecked()) && (!NumReactionRatesVariable->IsChecked()))
	{
		wxMessageBox(wxT("Please select at least one variable to output!"));
		wxBell();
	}   else {
				// Disable GUI
				FileOpenBttn->Enable(FALSE);
				DestBttn->Enable(FALSE);
				AboutBttn->Enable(FALSE);
				legacyVTKCheckbox->Enable(FALSE);
				legacyVTKRadioBox->Enable(FALSE);
				serialXMLCheckbox->Enable(FALSE);
				parallelXMLCheckbox->Enable(FALSE);
				cgnsCheckbox->Enable(FALSE);
				selectAllButton->Enable(FALSE);
				unselectAllButton->Enable(FALSE);
				VoidFractionVariable->Enable(FALSE);
				GasPressureVariable->Enable(FALSE);
				P_StarVariable->Enable(FALSE);
				GasVelocityVariable->Enable(FALSE);
				SolidPhaseVelocityVariable->Enable(FALSE);
				SolidPhaseDensityVariable->Enable(FALSE);
				TemperaturesVariable->Enable(FALSE);
				GasSpeciesVariable->Enable(FALSE);
				SolidPhaseSpeciesVariable->Enable(FALSE);
				Theta_MVariable->Enable(FALSE);
				ScalarsVariable->Enable(FALSE);
				NumReactionRatesVariable->Enable(FALSE);
				SelectAllOutputVariables->Enable(FALSE);
				UnselectAllOutputVariables->Enable(FALSE);
				BeginTSBox->Enable(FALSE);
				EndTSBox->Enable(FALSE);
				EnableSpecificTSRange->Enable(FALSE);
				enableLoggingCheckbox->Enable(FALSE);
				GoBttn->Enable(FALSE);
				CancelBttn->Enable(FALSE);
				exportGeometry->Enable(FALSE);
				exportBoundingBox->Enable(FALSE);    
				selectAllOptions->Enable(FALSE);
				unselectAllOptions->Enable(FALSE);

				// Disable unused progress gauges (whichever files have no associated output variables selected)
				if(VoidFractionVariable->GetValue() == TRUE)
				{	
					sp1Active = TRUE;
				}		
				if((GasPressureVariable->GetValue() == TRUE) || (P_StarVariable->GetValue() == TRUE))
				{
					sp2Active = TRUE;
				}
				if(GasVelocityVariable->GetValue() == TRUE)
				{
					sp3Active = TRUE;
				}
				if(SolidPhaseVelocityVariable->GetValue() == TRUE)
				{
					sp4Active = TRUE;
				}
				if(SolidPhaseDensityVariable->GetValue() == TRUE)
				{
					sp5Active = TRUE;
				}
				if(TemperaturesVariable->GetValue() == TRUE)
				{
					sp6Active = TRUE;
				}
				if((GasSpeciesVariable->GetValue() == TRUE) || (SolidPhaseSpeciesVariable->GetValue() == TRUE))
				{
					sp7Active = TRUE;
				}
				if(Theta_MVariable->GetValue() == TRUE)
				{
					sp8Active = TRUE;
				}
				if(ScalarsVariable->GetValue() == TRUE)
				{
					sp9Active = TRUE;
				}
				if(NumReactionRatesVariable->GetValue() == TRUE)
				{
					spaActive = TRUE;
				}

				if(VoidFractionVariable->GetValue() == FALSE)
				{
					static_text_1_TS->Enable(FALSE);
					SPGauge_1->Enable(FALSE);
					SPGauge_1->SetBackgroundColour(wxColour(194, 191, 165));
					static_text_1->Enable(FALSE);
				}		
				if((GasPressureVariable->GetValue() == FALSE) && (P_StarVariable->GetValue() == FALSE))
				{
					static_text_2_TS->Enable(FALSE);
					SPGauge_2->Enable(FALSE);
					SPGauge_2->SetBackgroundColour(wxColour(194, 191, 165));
					static_text_2->Enable(FALSE);
				}
				if(GasVelocityVariable->GetValue() == FALSE)
				{
					static_text_3_TS->Enable(FALSE);
					SPGauge_3->Enable(FALSE);
					SPGauge_3->SetBackgroundColour(wxColour(194, 191, 165));
					static_text_3->Enable(FALSE);
				}
				if(SolidPhaseVelocityVariable->GetValue() == FALSE)
				{
					static_text_4_TS->Enable(FALSE);
					SPGauge_4->Enable(FALSE);
					SPGauge_4->SetBackgroundColour(wxColour(194, 191, 165));
					static_text_4->Enable(FALSE);
				}
				if(SolidPhaseDensityVariable->GetValue() == FALSE)
				{
					static_text_5_TS->Enable(FALSE);
					SPGauge_5->Enable(FALSE);
					SPGauge_5->SetBackgroundColour(wxColour(194, 191, 165));
					static_text_5->Enable(FALSE);
				}
				if(TemperaturesVariable->GetValue() == FALSE)
				{
					static_text_6_TS->Enable(FALSE);
					SPGauge_6->Enable(FALSE);
					SPGauge_6->SetBackgroundColour(wxColour(194, 191, 165));
					static_text_6->Enable(FALSE);
				}
				if((GasSpeciesVariable->GetValue() == FALSE) && (SolidPhaseSpeciesVariable->GetValue() == FALSE))
				{
					static_text_7_TS->Enable(FALSE);
					SPGauge_7->Enable(FALSE);
					SPGauge_7->SetBackgroundColour(wxColour(194, 191, 165));
					static_text_7->Enable(FALSE);
				}
				if(Theta_MVariable->GetValue() == FALSE)
				{
					static_text_8_TS->Enable(FALSE);
					SPGauge_8->Enable(FALSE);
					SPGauge_8->SetBackgroundColour(wxColour(194, 191, 165));
					static_text_8->Enable(FALSE);
				}
				if(ScalarsVariable->GetValue() == FALSE)
				{
					static_text_9_TS->Enable(FALSE);
					SPGauge_9->Enable(FALSE);
					SPGauge_9->SetBackgroundColour(wxColour(194, 191, 165));
					static_text_9->Enable(FALSE);
				}
				if(NumReactionRatesVariable->GetValue() == FALSE)
				{
					static_text_a_TS->Enable(FALSE);
					SPGauge_a->Enable(FALSE);
					SPGauge_a->SetBackgroundColour(wxColour(194, 191, 165));
					static_text_a->Enable(FALSE);
				}

				totalTimeStepTally = BeginTSVal;

				/*// Tally number of output variables selected
				int OutputVariablesTotal = 0;
				if(VoidFractionVariable->GetValue() == TRUE)
				{
					OutputVariablesTotal++;
				}
				if((GasPressureVariable->GetValue() == TRUE) || (P_StarVariable->GetValue() == TRUE))
				{
					OutputVariablesTotal++;
				}
				if(GasVelocityVariable->GetValue() == TRUE)
				{
					OutputVariablesTotal++;
				}
				if(SolidPhaseVelocityVariable->GetValue() == TRUE)
				{
					OutputVariablesTotal++;
				}
				if(SolidPhaseDensityVariable->GetValue() == TRUE)
				{
					OutputVariablesTotal++;
				}
				if(TemperaturesVariable->GetValue() == TRUE)
				{
					OutputVariablesTotal++;
				}
				if((GasSpeciesVariable->GetValue() == TRUE) || (SolidPhaseSpeciesVariable->GetValue() == TRUE))
				{
					OutputVariablesTotal++;
				}
				if(Theta_MVariable->GetValue() == TRUE)
				{
					OutputVariablesTotal++;
				}
				if(ScalarsVariable->GetValue() == TRUE)
				{
					OutputVariablesTotal++;
				}
				if(NumReactionRatesVariable->GetValue() == TRUE)
				{
					OutputVariablesTotal++;
				}*/

				// Set Value Range
				ProgressGauge->SetRange((EndTSVal-BeginTSVal) * formatTally);

				text_ctrl_statusReport->AppendText(wxT("Translation began at ") + wxNow() + wxT(".\n"));

				// Begin translation
				SetOutputVariableFlags();

				// Load file writer
				//	if type (param 4) = 1, output = ASCII legacy format (.vtk)
				//  if type (param 4) = 2, output = binary legacy format (.vtk)
				//  if type (param 4) = 3, output = XML serial format (.vtu)
				//  if type (param 4) = 4, output = XML parallel format (.pvtu)
				// Write all needed files
				if(legacyVTKOutput)
				{
					formatCurrent++;
					FileOutputDisplay.Printf(wxT("Currently working on %i of %i formats."), formatCurrent, formatTally);
					outputFormatText->SetLabel(FileOutputDisplay);

					// Zero out Individual Progress Gauges and Timestep Displays
					wxString InitialTSDisplay01;
					InitialTSDisplay01.Printf(wxT("0/%i Timesteps"), sp1TimeSteps);
					static_text_1_TS->SetLabel(InitialTSDisplay01);
					SPGauge_1->SetValue(0);
					wxString InitialTSDisplay02;
					InitialTSDisplay02.Printf(wxT("0/%i Timesteps"), sp2TimeSteps);
					static_text_2_TS->SetLabel(InitialTSDisplay02);
					SPGauge_2->SetValue(0);
					wxString InitialTSDisplay03;
					InitialTSDisplay03.Printf(wxT("0/%i Timesteps"), sp3TimeSteps);
					static_text_3_TS->SetLabel(InitialTSDisplay03);
					SPGauge_3->SetValue(0);
					wxString InitialTSDisplay04;
					InitialTSDisplay04.Printf(wxT("0/%i Timesteps"), sp4TimeSteps);
					static_text_4_TS->SetLabel(InitialTSDisplay04);
					SPGauge_4->SetValue(0);
					wxString InitialTSDisplay05;
					InitialTSDisplay05.Printf(wxT("0/%i Timesteps"), sp5TimeSteps);
					static_text_5_TS->SetLabel(InitialTSDisplay05);
					SPGauge_5->SetValue(0);
					wxString InitialTSDisplay06;
					InitialTSDisplay06.Printf(wxT("0/%i Timesteps"), sp6TimeSteps);
					static_text_6_TS->SetLabel(InitialTSDisplay06);
					SPGauge_6->SetValue(0);				
					wxString InitialTSDisplay07;
					InitialTSDisplay07.Printf(wxT("0/%i Timesteps"), sp7TimeSteps);
					static_text_7_TS->SetLabel(InitialTSDisplay07);
					SPGauge_7->SetValue(0);				
					wxString InitialTSDisplay08;
					InitialTSDisplay08.Printf(wxT("0/%i Timesteps"), sp8TimeSteps);
					static_text_8_TS->SetLabel(InitialTSDisplay08);
					SPGauge_8->SetValue(0);				
					wxString InitialTSDisplay09;
					InitialTSDisplay09.Printf(wxT("0/%i Timesteps"), sp9TimeSteps);
					static_text_9_TS->SetLabel(InitialTSDisplay09);
					SPGauge_9->SetValue(0);
					wxString InitialTSDisplaya;
					InitialTSDisplaya.Printf(wxT("0/%i Timesteps"), spaTimeSteps);
					static_text_a_TS->SetLabel(InitialTSDisplaya);
					SPGauge_a->SetValue(0);

					if(legacyVTKRadioBox->GetSelection()==0)
					{
						GridWriterFormatFlag = 1;
					} else {
						GridWriterFormatFlag = 2;
					}
					// Start legacy VTK translation
					wxString TSDisplay, statusReportStr;
					statusReportStr.Printf(wxT("Beginning translation to Legacy VTK format.\n"));
					text_ctrl_statusReport->AppendText(statusReportStr);
					for(int i = BeginTSVal; i <= EndTSVal; i++)
					{	
						string myString;
						myString = inFilename;
						myString.replace(myString.find_last_of("."), 4, ".sp1");
						strcpy(inFilename, myString.c_str());
						totalTimeStepTally++;

						// SP1 timestep gauge
						if (sp1Active == TRUE) {
							if (sp1TimeSteps > 0) {
								diffTS = MaxTimeSteps/sp1TimeSteps;
								actualTS = i/diffTS;
							}
							else
								actualTS = sp1TimeSteps;
							TSDisplay.Printf(wxT("%i/%i Timesteps"), actualTS, sp1TimeSteps);
							static_text_1_TS->SetLabel(TSDisplay);
							if (actualTS <= sp1TimeSteps) {
								if (sp1TimeSteps > 0)
									SPGauge_1->SetValue(actualTS);
								else
									SPGauge_1->SetValue(actualTS+1);
							}
						}

						// SP2 timestep gauge
						if (sp2Active == TRUE) {
							if (sp2TimeSteps > 0) {
								diffTS = MaxTimeSteps/sp2TimeSteps;
								actualTS = i/diffTS;
							}
							else
								actualTS = sp2TimeSteps;
							TSDisplay.Printf(wxT("%i/%i Timesteps"), actualTS, sp2TimeSteps);
							static_text_2_TS->SetLabel(TSDisplay);
							if (actualTS <= sp2TimeSteps) {
								if (sp2TimeSteps > 0) 
									SPGauge_2->SetValue(actualTS);
								else
									SPGauge_2->SetValue(actualTS+1);
							}
						}

						// SP3 timestep gauge
						if (sp3Active == TRUE) {
							if (sp3TimeSteps > 0) {
								diffTS = MaxTimeSteps/sp3TimeSteps;
								actualTS = i/diffTS;
							}
							else
								actualTS = sp3TimeSteps;
							TSDisplay.Printf(wxT("%i/%i Timesteps"), actualTS, sp3TimeSteps);
							static_text_3_TS->SetLabel(TSDisplay);
							if (actualTS <= sp3TimeSteps) {
								if (sp3TimeSteps > 0)
									SPGauge_3->SetValue(actualTS);
								else
									SPGauge_3->SetValue(actualTS+1);
							}
						}

						// SP4 timestep gauge
						if (sp4Active == TRUE) {
							if (sp4TimeSteps > 0) {
								diffTS = MaxTimeSteps/sp4TimeSteps;
								actualTS = i/diffTS;
							}
							else
								actualTS = sp4TimeSteps;
							TSDisplay.Printf(wxT("%i/%i Timesteps"), actualTS, sp4TimeSteps);
							static_text_4_TS->SetLabel(TSDisplay);
							if (actualTS <= sp4TimeSteps) {
								if (sp4TimeSteps > 0)
									SPGauge_4->SetValue(actualTS);
								else
									SPGauge_4->SetValue(actualTS+1);
							}
						}

						// SP5 timestep gauge
						if (sp5Active == TRUE) {
							if (sp5TimeSteps > 0) {
								diffTS = MaxTimeSteps/sp5TimeSteps;
								actualTS = i/diffTS;
							}
							else
								actualTS = sp5TimeSteps;
							TSDisplay.Printf(wxT("%i/%i Timesteps"), actualTS, sp5TimeSteps);
							static_text_5_TS->SetLabel(TSDisplay);
							if (actualTS <= sp5TimeSteps) {
								if (sp5TimeSteps > 0)
									SPGauge_5->SetValue(actualTS);
								else
									SPGauge_5->SetValue(actualTS+1);
							}
						}

						// SP6 timestep gauge
						if (sp6Active == TRUE) {
							if (sp6TimeSteps > 0) {
								diffTS = MaxTimeSteps/sp6TimeSteps;
								actualTS = i/diffTS;
							}
							else
								actualTS = sp6TimeSteps;
							TSDisplay.Printf(wxT("%i/%i Timesteps"), actualTS, sp6TimeSteps);
							static_text_6_TS->SetLabel(TSDisplay);
							if (actualTS <= sp6TimeSteps) {
								if (sp6TimeSteps > 0)
									SPGauge_6->SetValue(actualTS);
								else
									SPGauge_6->SetValue(actualTS+1);
							}
						}

						// SP7 timestep gauge
						if (sp7Active == TRUE) {
							if (sp7TimeSteps > 0) {
								diffTS = MaxTimeSteps/sp7TimeSteps;
								actualTS = i/diffTS;
							}
							else
								actualTS = sp7TimeSteps;
							TSDisplay.Printf(wxT("%i/%i Timesteps"), actualTS, sp7TimeSteps);
							static_text_7_TS->SetLabel(TSDisplay);
							if (actualTS <= sp7TimeSteps) {
								if (sp7TimeSteps > 0)
									SPGauge_7->SetValue(actualTS);
								else
									SPGauge_7->SetValue(actualTS+1);
							}
						}

						// SP8 timestep gauge
						if (sp8Active == TRUE) {
							if (sp8TimeSteps > 0) {
								diffTS = MaxTimeSteps/sp8TimeSteps;
								actualTS = i/diffTS;
							}
							else
								actualTS = sp8TimeSteps;
							TSDisplay.Printf(wxT("%i/%i Timesteps"), actualTS, sp8TimeSteps);
							static_text_8_TS->SetLabel(TSDisplay);
							if (actualTS <= sp8TimeSteps) {
								if (sp8TimeSteps > 0)
									SPGauge_8->SetValue(actualTS);
								else
									SPGauge_8->SetValue(actualTS+1);
							}
						}

						// SP9 timestep gauge
						if (sp9Active == TRUE) {
							if (sp9TimeSteps > 0) {
								diffTS = MaxTimeSteps/sp9TimeSteps;
								actualTS = i/diffTS;
							}
							else
								actualTS = sp9TimeSteps;
							TSDisplay.Printf(wxT("%i/%i Timesteps"), actualTS, sp9TimeSteps);
							static_text_9_TS->SetLabel(TSDisplay);
							if (actualTS <= sp9TimeSteps) {
								if (sp9TimeSteps > 0)
									SPGauge_9->SetValue(actualTS);
								else
									SPGauge_9->SetValue(actualTS+1);
							}
						}

						// SPA timestep gauge
						if (spaActive == TRUE) {
							if (spaTimeSteps > 0) {
								diffTS = MaxTimeSteps/spaTimeSteps;
								actualTS = i/diffTS;
							}
							else
								actualTS = spaTimeSteps;
							TSDisplay.Printf(wxT("%i/%i Timesteps"), actualTS, spaTimeSteps);
							static_text_a_TS->SetLabel(TSDisplay);
							if (actualTS <= spaTimeSteps) {
								if (spaTimeSteps > 0)
									SPGauge_a->SetValue(actualTS);
								else
									SPGauge_a->SetValue(actualTS+1);
							}
						}

						ProgressGauge->SetValue(totalTimeStepTally);
						wxWindow::Update();
						UnstructuredGridWriter((char *)ProjectName.c_str(), (char *)Dir, i, MaxTimeSteps, GridWriterFormatFlag, &resH, &spH, &mfD); 
						statusReportStr.Printf(wxT("Timestep %i completed in Legacy VTK format.\n"), i);
						text_ctrl_statusReport->AppendText(statusReportStr);
						if(enableLoggingCheckbox->IsChecked())
						{
							text_ctrl_statusReport->SaveFile(logFile);
						}
						wxYield();
					}	
					statusReportStr.Printf(wxT("Translation to Legacy VTK format complete.\n"));
					text_ctrl_statusReport->AppendText(statusReportStr);
				}
				if(serialXMLOutput)
				{	
					GridWriterFormatFlag = 3;	
					formatCurrent++;
					FileOutputDisplay.Printf(wxT("Currently working on %i of %i formats."), formatCurrent, formatTally);
					outputFormatText->SetLabel(FileOutputDisplay);

					// Zero out Individual Progress Gauges and Timestep Displays
					wxString InitialTSDisplay01;
					InitialTSDisplay01.Printf(wxT("0/%i Timesteps"), sp1TimeSteps);
					static_text_1_TS->SetLabel(InitialTSDisplay01);
					SPGauge_1->SetValue(0);
					wxString InitialTSDisplay02;
					InitialTSDisplay02.Printf(wxT("0/%i Timesteps"), sp2TimeSteps);
					static_text_2_TS->SetLabel(InitialTSDisplay02);
					SPGauge_2->SetValue(0);
					wxString InitialTSDisplay03;
					InitialTSDisplay03.Printf(wxT("0/%i Timesteps"), sp3TimeSteps);
					static_text_3_TS->SetLabel(InitialTSDisplay03);
					SPGauge_3->SetValue(0);
					wxString InitialTSDisplay04;
					InitialTSDisplay04.Printf(wxT("0/%i Timesteps"), sp4TimeSteps);
					static_text_4_TS->SetLabel(InitialTSDisplay04);
					SPGauge_4->SetValue(0);
					wxString InitialTSDisplay05;
					InitialTSDisplay05.Printf(wxT("0/%i Timesteps"), sp5TimeSteps);
					static_text_5_TS->SetLabel(InitialTSDisplay05);
					SPGauge_5->SetValue(0);
					wxString InitialTSDisplay06;
					InitialTSDisplay06.Printf(wxT("0/%i Timesteps"), sp6TimeSteps);
					static_text_6_TS->SetLabel(InitialTSDisplay06);
					SPGauge_6->SetValue(0);				
					wxString InitialTSDisplay07;
					InitialTSDisplay07.Printf(wxT("0/%i Timesteps"), sp7TimeSteps);
					static_text_7_TS->SetLabel(InitialTSDisplay07);
					SPGauge_7->SetValue(0);				
					wxString InitialTSDisplay08;
					InitialTSDisplay08.Printf(wxT("0/%i Timesteps"), sp8TimeSteps);
					static_text_8_TS->SetLabel(InitialTSDisplay08);
					SPGauge_8->SetValue(0);				
					wxString InitialTSDisplay09;
					InitialTSDisplay09.Printf(wxT("0/%i Timesteps"), sp9TimeSteps);
					static_text_9_TS->SetLabel(InitialTSDisplay09);
					SPGauge_9->SetValue(0);
					wxString InitialTSDisplaya;
					InitialTSDisplaya.Printf(wxT("0/%i Timesteps"), spaTimeSteps);
					static_text_a_TS->SetLabel(InitialTSDisplaya);
					SPGauge_a->SetValue(0);

					// serial XML translation
					wxString TSDisplay, statusReportStr;
					statusReportStr.Printf(wxT("Beginning translation to Serial XML format.\n"));
					text_ctrl_statusReport->AppendText(statusReportStr);
					for(int i = BeginTSVal; i <= EndTSVal; i++)
					{	
						string myString;
						myString = inFilename;
						myString.replace(myString.find_last_of("."), 4, ".sp1");
						strcpy(inFilename, myString.c_str());
						totalTimeStepTally++;

						// SP1 timestep gauge
						if (sp1Active == TRUE) {
							if (sp1TimeSteps > 0) {
								diffTS = MaxTimeSteps/sp1TimeSteps;
								actualTS = i/diffTS;
							}
							else
								actualTS = sp1TimeSteps;
							TSDisplay.Printf(wxT("%i/%i Timesteps"), actualTS, sp1TimeSteps);
							static_text_1_TS->SetLabel(TSDisplay);
							if (actualTS <= sp1TimeSteps) {
								if (sp1TimeSteps > 0)
									SPGauge_1->SetValue(actualTS);
								else
									SPGauge_1->SetValue(actualTS+1);
							}
						}

						// SP2 timestep gauge
						if (sp2Active == TRUE) {
							if (sp2TimeSteps > 0) {
								diffTS = MaxTimeSteps/sp2TimeSteps;
								actualTS = i/diffTS;
							}
							else
								actualTS = sp2TimeSteps;
							TSDisplay.Printf(wxT("%i/%i Timesteps"), actualTS, sp2TimeSteps);
							static_text_2_TS->SetLabel(TSDisplay);
							if (actualTS <= sp2TimeSteps) {
								if (sp2TimeSteps > 0) 
									SPGauge_2->SetValue(actualTS);
								else
									SPGauge_2->SetValue(actualTS+1);
							}
						}

						// SP3 timestep gauge
						if (sp3Active == TRUE) {
							if (sp3TimeSteps > 0) {
								diffTS = MaxTimeSteps/sp3TimeSteps;
								actualTS = i/diffTS;
							}
							else
								actualTS = sp3TimeSteps;
							TSDisplay.Printf(wxT("%i/%i Timesteps"), actualTS, sp3TimeSteps);
							static_text_3_TS->SetLabel(TSDisplay);
							if (actualTS <= sp3TimeSteps) {
								if (sp3TimeSteps > 0)
									SPGauge_3->SetValue(actualTS);
								else
									SPGauge_3->SetValue(actualTS+1);
							}
						}

						// SP4 timestep gauge
						if (sp4Active == TRUE) {
							if (sp4TimeSteps > 0) {
								diffTS = MaxTimeSteps/sp4TimeSteps;
								actualTS = i/diffTS;
							}
							else
								actualTS = sp4TimeSteps;
							TSDisplay.Printf(wxT("%i/%i Timesteps"), actualTS, sp4TimeSteps);
							static_text_4_TS->SetLabel(TSDisplay);
							if (actualTS <= sp4TimeSteps) {
								if (sp4TimeSteps > 0)
									SPGauge_4->SetValue(actualTS);
								else
									SPGauge_4->SetValue(actualTS+1);
							}
						}

						// SP5 timestep gauge
						if (sp5Active == TRUE) {
							if (sp5TimeSteps > 0) {
								diffTS = MaxTimeSteps/sp5TimeSteps;
								actualTS = i/diffTS;
							}
							else
								actualTS = sp5TimeSteps;
							TSDisplay.Printf(wxT("%i/%i Timesteps"), actualTS, sp5TimeSteps);
							static_text_5_TS->SetLabel(TSDisplay);
							if (actualTS <= sp5TimeSteps) {
								if (sp5TimeSteps > 0)
									SPGauge_5->SetValue(actualTS);
								else
									SPGauge_5->SetValue(actualTS+1);
							}
						}

						// SP6 timestep gauge
						if (sp6Active == TRUE) {
							if (sp6TimeSteps > 0) {
								diffTS = MaxTimeSteps/sp6TimeSteps;
								actualTS = i/diffTS;
							}
							else
								actualTS = sp6TimeSteps;
							TSDisplay.Printf(wxT("%i/%i Timesteps"), actualTS, sp6TimeSteps);
							static_text_6_TS->SetLabel(TSDisplay);
							if (actualTS <= sp6TimeSteps) {
								if (sp6TimeSteps > 0)
									SPGauge_6->SetValue(actualTS);
								else
									SPGauge_6->SetValue(actualTS+1);
							}
						}

						// SP7 timestep gauge
						if (sp7Active == TRUE) {
							if (sp7TimeSteps > 0) {
								diffTS = MaxTimeSteps/sp7TimeSteps;
								actualTS = i/diffTS;
							}
							else
								actualTS = sp7TimeSteps;
							TSDisplay.Printf(wxT("%i/%i Timesteps"), actualTS, sp7TimeSteps);
							static_text_7_TS->SetLabel(TSDisplay);
							if (actualTS <= sp7TimeSteps) {
								if (sp7TimeSteps > 0)
									SPGauge_7->SetValue(actualTS);
								else
									SPGauge_7->SetValue(actualTS+1);
							}
						}

						// SP8 timestep gauge
						if (sp8Active == TRUE) {
							if (sp8TimeSteps > 0) {
								diffTS = MaxTimeSteps/sp8TimeSteps;
								actualTS = i/diffTS;
							}
							else
								actualTS = sp8TimeSteps;
							TSDisplay.Printf(wxT("%i/%i Timesteps"), actualTS, sp8TimeSteps);
							static_text_8_TS->SetLabel(TSDisplay);
							if (actualTS <= sp8TimeSteps) {
								if (sp8TimeSteps > 0)
									SPGauge_8->SetValue(actualTS);
								else
									SPGauge_8->SetValue(actualTS+1);
							}
						}

						// SP9 timestep gauge
						if (sp9Active == TRUE) {
							if (sp9TimeSteps > 0) {
								diffTS = MaxTimeSteps/sp9TimeSteps;
								actualTS = i/diffTS;
							}
							else
								actualTS = sp9TimeSteps;
							TSDisplay.Printf(wxT("%i/%i Timesteps"), actualTS, sp9TimeSteps);
							static_text_9_TS->SetLabel(TSDisplay);
							if (actualTS <= sp9TimeSteps) {
								if (sp9TimeSteps > 0)
									SPGauge_9->SetValue(actualTS);
								else
									SPGauge_9->SetValue(actualTS+1);
							}
						}

						// SPA timestep gauge
						if (spaActive == TRUE) {
							if (spaTimeSteps > 0) {
								diffTS = MaxTimeSteps/spaTimeSteps;
								actualTS = i/diffTS;
							}
							else
								actualTS = spaTimeSteps;
							TSDisplay.Printf(wxT("%i/%i Timesteps"), actualTS, spaTimeSteps);
							static_text_a_TS->SetLabel(TSDisplay);
							if (actualTS <= spaTimeSteps) {
								if (spaTimeSteps > 0)
									SPGauge_a->SetValue(actualTS);
								else
									SPGauge_a->SetValue(actualTS+1);
							}
						}

						ProgressGauge->SetValue(totalTimeStepTally);
						wxWindow::Update();
						UnstructuredGridWriter((char *)ProjectName.c_str(), (char *)Dir ,i , MaxTimeSteps, GridWriterFormatFlag, &resH, &spH, &mfD); 
						statusReportStr.Printf(wxT("Timestep %i completed in Serial XML format.\n"), i);
						text_ctrl_statusReport->AppendText(statusReportStr);
						if(enableLoggingCheckbox->IsChecked())
						{
							text_ctrl_statusReport->SaveFile(logFile);
						}
						wxYield();
					}		
					statusReportStr.Printf(wxT("Translation to Serial XML format complete.\n"));
					text_ctrl_statusReport->AppendText(statusReportStr);
				}
				if(parallelXMLOutput)
				{
					GridWriterFormatFlag = 4;	
					formatCurrent++;
					FileOutputDisplay.Printf(wxT("Currently working on %i of %i formats."), formatCurrent, formatTally);
					outputFormatText->SetLabel(FileOutputDisplay);
					
					// Zero out Individual Progress Gauges and Timestep Displays
					wxString InitialTSDisplay01;
					InitialTSDisplay01.Printf(wxT("0/%i Timesteps"), sp1TimeSteps);
					static_text_1_TS->SetLabel(InitialTSDisplay01);
					SPGauge_1->SetValue(0);
					wxString InitialTSDisplay02;
					InitialTSDisplay02.Printf(wxT("0/%i Timesteps"), sp2TimeSteps);
					static_text_2_TS->SetLabel(InitialTSDisplay02);
					SPGauge_2->SetValue(0);
					wxString InitialTSDisplay03;
					InitialTSDisplay03.Printf(wxT("0/%i Timesteps"), sp3TimeSteps);
					static_text_3_TS->SetLabel(InitialTSDisplay03);
					SPGauge_3->SetValue(0);
					wxString InitialTSDisplay04;
					InitialTSDisplay04.Printf(wxT("0/%i Timesteps"), sp4TimeSteps);
					static_text_4_TS->SetLabel(InitialTSDisplay04);
					SPGauge_4->SetValue(0);
					wxString InitialTSDisplay05;
					InitialTSDisplay05.Printf(wxT("0/%i Timesteps"), sp5TimeSteps);
					static_text_5_TS->SetLabel(InitialTSDisplay05);
					SPGauge_5->SetValue(0);
					wxString InitialTSDisplay06;
					InitialTSDisplay06.Printf(wxT("0/%i Timesteps"), sp6TimeSteps);
					static_text_6_TS->SetLabel(InitialTSDisplay06);
					SPGauge_6->SetValue(0);				
					wxString InitialTSDisplay07;
					InitialTSDisplay07.Printf(wxT("0/%i Timesteps"), sp7TimeSteps);
					static_text_7_TS->SetLabel(InitialTSDisplay07);
					SPGauge_7->SetValue(0);				
					wxString InitialTSDisplay08;
					InitialTSDisplay08.Printf(wxT("0/%i Timesteps"), sp8TimeSteps);
					static_text_8_TS->SetLabel(InitialTSDisplay08);
					SPGauge_8->SetValue(0);				
					wxString InitialTSDisplay09;
					InitialTSDisplay09.Printf(wxT("0/%i Timesteps"), sp9TimeSteps);
					static_text_9_TS->SetLabel(InitialTSDisplay09);
					SPGauge_9->SetValue(0);
					wxString InitialTSDisplaya;
					InitialTSDisplaya.Printf(wxT("0/%i Timesteps"), spaTimeSteps);
					static_text_a_TS->SetLabel(InitialTSDisplaya);
					SPGauge_a->SetValue(0);
		
					// parallel XML format
					wxString TSDisplay, statusReportStr;
					statusReportStr.Printf(wxT("Beginning translation to Parallel XML format.\n"));
					text_ctrl_statusReport->AppendText(statusReportStr);
					for(int i = BeginTSVal; i <= EndTSVal; i++)
					{	
						string myString;
						myString = inFilename;
						myString.replace(myString.find_last_of("."), 4, ".sp1");
						strcpy(inFilename, myString.c_str());
						totalTimeStepTally++;
                                                
						// SP1 timestep gauge
						if (sp1Active == TRUE) {
							if (sp1TimeSteps > 0) {
								diffTS = MaxTimeSteps/sp1TimeSteps;
								actualTS = i/diffTS;
							}
							else
								actualTS = sp1TimeSteps;
							TSDisplay.Printf(wxT("%i/%i Timesteps"), actualTS, sp1TimeSteps);
							static_text_1_TS->SetLabel(TSDisplay);
							if (actualTS <= sp1TimeSteps) {
								if (sp1TimeSteps > 0)
									SPGauge_1->SetValue(actualTS);
								else
									SPGauge_1->SetValue(actualTS+1);
							}
						}

						// SP2 timestep gauge
						if (sp2Active == TRUE) {
							if (sp2TimeSteps > 0) {
								diffTS = MaxTimeSteps/sp2TimeSteps;
								actualTS = i/diffTS;
							}
							else
								actualTS = sp2TimeSteps;
							TSDisplay.Printf(wxT("%i/%i Timesteps"), actualTS, sp2TimeSteps);
							static_text_2_TS->SetLabel(TSDisplay);
							if (actualTS <= sp2TimeSteps) {
								if (sp2TimeSteps > 0) 
									SPGauge_2->SetValue(actualTS);
								else
									SPGauge_2->SetValue(actualTS+1);
							}
						}

						// SP3 timestep gauge
						if (sp3Active == TRUE) {
							if (sp3TimeSteps > 0) {
								diffTS = MaxTimeSteps/sp3TimeSteps;
								actualTS = i/diffTS;
							}
							else
								actualTS = sp3TimeSteps;
							TSDisplay.Printf(wxT("%i/%i Timesteps"), actualTS, sp3TimeSteps);
							static_text_3_TS->SetLabel(TSDisplay);
							if (actualTS <= sp3TimeSteps) {
								if (sp3TimeSteps > 0)
									SPGauge_3->SetValue(actualTS);
								else
									SPGauge_3->SetValue(actualTS+1);
							}
						}

						// SP4 timestep gauge
						if (sp4Active == TRUE) {
							if (sp4TimeSteps > 0) {
								diffTS = MaxTimeSteps/sp4TimeSteps;
								actualTS = i/diffTS;
							}
							else
								actualTS = sp4TimeSteps;
							TSDisplay.Printf(wxT("%i/%i Timesteps"), actualTS, sp4TimeSteps);
							static_text_4_TS->SetLabel(TSDisplay);
							if (actualTS <= sp4TimeSteps) {
								if (sp4TimeSteps > 0)
									SPGauge_4->SetValue(actualTS);
								else
									SPGauge_4->SetValue(actualTS+1);
							}
						}

						// SP5 timestep gauge
						if (sp5Active == TRUE) {
							if (sp5TimeSteps > 0) {
								diffTS = MaxTimeSteps/sp5TimeSteps;
								actualTS = i/diffTS;
							}
							else
								actualTS = sp5TimeSteps;
							TSDisplay.Printf(wxT("%i/%i Timesteps"), actualTS, sp5TimeSteps);
							static_text_5_TS->SetLabel(TSDisplay);
							if (actualTS <= sp5TimeSteps) {
								if (sp5TimeSteps > 0)
									SPGauge_5->SetValue(actualTS);
								else
									SPGauge_5->SetValue(actualTS+1);
							}
						}

						// SP6 timestep gauge
						if (sp6Active == TRUE) {
							if (sp6TimeSteps > 0) {
								diffTS = MaxTimeSteps/sp6TimeSteps;
								actualTS = i/diffTS;
							}
							else
								actualTS = sp6TimeSteps;
							TSDisplay.Printf(wxT("%i/%i Timesteps"), actualTS, sp6TimeSteps);
							static_text_6_TS->SetLabel(TSDisplay);
							if (actualTS <= sp6TimeSteps) {
								if (sp6TimeSteps > 0)
									SPGauge_6->SetValue(actualTS);
								else
									SPGauge_6->SetValue(actualTS+1);
							}
						}

						// SP7 timestep gauge
						if (sp7Active == TRUE) {
							if (sp7TimeSteps > 0) {
								diffTS = MaxTimeSteps/sp7TimeSteps;
								actualTS = i/diffTS;
							}
							else
								actualTS = sp7TimeSteps;
							TSDisplay.Printf(wxT("%i/%i Timesteps"), actualTS, sp7TimeSteps);
							static_text_7_TS->SetLabel(TSDisplay);
							if (actualTS <= sp7TimeSteps) {
								if (sp7TimeSteps > 0)
									SPGauge_7->SetValue(actualTS);
								else
									SPGauge_7->SetValue(actualTS+1);
							}
						}

						// SP8 timestep gauge
						if (sp8Active == TRUE) {
							if (sp8TimeSteps > 0) {
								diffTS = MaxTimeSteps/sp8TimeSteps;
								actualTS = i/diffTS;
							}
							else
								actualTS = sp8TimeSteps;
							TSDisplay.Printf(wxT("%i/%i Timesteps"), actualTS, sp8TimeSteps);
							static_text_8_TS->SetLabel(TSDisplay);
							if (actualTS <= sp8TimeSteps) {
								if (sp8TimeSteps > 0)
									SPGauge_8->SetValue(actualTS);
								else
									SPGauge_8->SetValue(actualTS+1);
							}
						}

						// SP9 timestep gauge
						if (sp9Active == TRUE) {
							if (sp9TimeSteps > 0) {
								diffTS = MaxTimeSteps/sp9TimeSteps;
								actualTS = i/diffTS;
							}
							else
								actualTS = sp9TimeSteps;
							TSDisplay.Printf(wxT("%i/%i Timesteps"), actualTS, sp9TimeSteps);
							static_text_9_TS->SetLabel(TSDisplay);
							if (actualTS <= sp9TimeSteps) {
								if (sp9TimeSteps > 0)
									SPGauge_9->SetValue(actualTS);
								else
									SPGauge_9->SetValue(actualTS+1);
							}
						}

						// SPA timestep gauge
						if (spaActive == TRUE) {
							if (spaTimeSteps > 0) {
								diffTS = MaxTimeSteps/spaTimeSteps;
								actualTS = i/diffTS;
							}
							else
								actualTS = spaTimeSteps;
							TSDisplay.Printf(wxT("%i/%i Timesteps"), actualTS, spaTimeSteps);
							static_text_a_TS->SetLabel(TSDisplay);
							if (actualTS <= spaTimeSteps) {
								if (spaTimeSteps > 0)
									SPGauge_a->SetValue(actualTS);
								else
									SPGauge_a->SetValue(actualTS+1);
							}
						}

						ProgressGauge->SetValue(totalTimeStepTally);
						wxWindow::Update();
						UnstructuredGridWriter((char *)ProjectName.c_str(), (char *)Dir, i, MaxTimeSteps, GridWriterFormatFlag, &resH, &spH, &mfD); 
						statusReportStr.Printf(wxT("Timestep %i completed in Parallel XML format.\n"), i);
						text_ctrl_statusReport->AppendText(statusReportStr);
						if(enableLoggingCheckbox->IsChecked())
							{
							text_ctrl_statusReport->SaveFile(logFile);
						}
						wxYield();
					}			
					statusReportStr.Printf(wxT("Translation to Parallel XML format complete.\n"));
					text_ctrl_statusReport->AppendText(statusReportStr);
				}
				if(CGNSOutput)
				{	
					formatCurrent++;
					FileOutputDisplay.Printf(wxT("Currently working on %i of %i formats."), formatCurrent, formatTally);
					outputFormatText->SetLabel(FileOutputDisplay);
					
					// Zero out Individual Progress Gauges and Timestep Displays
					wxString InitialTSDisplay01;
					InitialTSDisplay01.Printf(wxT("0/%i Timesteps"), sp1TimeSteps);
					static_text_1_TS->SetLabel(InitialTSDisplay01);
					SPGauge_1->SetValue(0);
					wxString InitialTSDisplay02;
					InitialTSDisplay02.Printf(wxT("0/%i Timesteps"), sp2TimeSteps);
					static_text_2_TS->SetLabel(InitialTSDisplay02);
					SPGauge_2->SetValue(0);
					wxString InitialTSDisplay03;
					InitialTSDisplay03.Printf(wxT("0/%i Timesteps"), sp3TimeSteps);
					static_text_3_TS->SetLabel(InitialTSDisplay03);
					SPGauge_3->SetValue(0);
					wxString InitialTSDisplay04;
					InitialTSDisplay04.Printf(wxT("0/%i Timesteps"), sp4TimeSteps);
					static_text_4_TS->SetLabel(InitialTSDisplay04);
					SPGauge_4->SetValue(0);
					wxString InitialTSDisplay05;
					InitialTSDisplay05.Printf(wxT("0/%i Timesteps"), sp5TimeSteps);
					static_text_5_TS->SetLabel(InitialTSDisplay05);
					SPGauge_5->SetValue(0);
					wxString InitialTSDisplay06;
					InitialTSDisplay06.Printf(wxT("0/%i Timesteps"), sp6TimeSteps);
					static_text_6_TS->SetLabel(InitialTSDisplay06);
					SPGauge_6->SetValue(0);				
					wxString InitialTSDisplay07;
					InitialTSDisplay07.Printf(wxT("0/%i Timesteps"), sp7TimeSteps);
					static_text_7_TS->SetLabel(InitialTSDisplay07);
					SPGauge_7->SetValue(0);				
					wxString InitialTSDisplay08;
					InitialTSDisplay08.Printf(wxT("0/%i Timesteps"), sp8TimeSteps);
					static_text_8_TS->SetLabel(InitialTSDisplay08);
					SPGauge_8->SetValue(0);				
					wxString InitialTSDisplay09;
					InitialTSDisplay09.Printf(wxT("0/%i Timesteps"), sp9TimeSteps);
					static_text_9_TS->SetLabel(InitialTSDisplay09);
					SPGauge_9->SetValue(0);
					wxString InitialTSDisplaya;
					InitialTSDisplaya.Printf(wxT("0/%i Timesteps"), spaTimeSteps);
					static_text_a_TS->SetLabel(InitialTSDisplaya);
					SPGauge_a->SetValue(0);

					// CGNS
					wxString TSDisplay, statusReportStr;
					statusReportStr.Printf(wxT("Beginning sp1 translation to CGNS format.\n"));
					text_ctrl_statusReport->AppendText(statusReportStr);
					for(int i = BeginTSVal; i <= EndTSVal; i++)
					{	
						string myString;
						myString = inFilename;
						myString.replace(myString.find_last_of("."), 4, ".sp1");
						strcpy(inFilename, myString.c_str());
						TSDisplay.Printf(wxT("%i/%i Timesteps"), i, MaxTimeSteps);
						static_text_1_TS->SetLabel(TSDisplay);
						SPGauge_1->SetValue(i);
						totalTimeStepTally++;
						ProgressGauge->SetValue(totalTimeStepTally);
						wxWindow::Update();
						UnstructuredGridWriter((char *)ProjectName.c_str(), (char *)Dir, i, MaxTimeSteps, GridWriterFormatFlag, &resH, &spH, &mfD); 
						statusReportStr.Printf(wxT("Timestep %i of sp1 file completed in CGNS format.\n"), i);
						text_ctrl_statusReport->AppendText(statusReportStr);
						if(enableLoggingCheckbox->IsChecked())
						{
							text_ctrl_statusReport->SaveFile(logFile);
						}
						wxYield();
					}	
					statusReportStr.Printf(wxT("sp1 translation to CGNS format complete.\n"));
					text_ctrl_statusReport->AppendText(statusReportStr);
				}

				// Export geometry
				if (exportGeometry->IsChecked()) {
					wxString statusReportStr;
					statusReportStr.Printf(wxT("MFIX geometry exported to VTK.\n"));
					text_ctrl_statusReport->AppendText(statusReportStr);
				}
				
				text_ctrl_statusReport->AppendText(wxT("Translation complete at ") + wxNow() + wxT(".\n"));
				if(enableLoggingCheckbox->IsChecked())
				{
					text_ctrl_statusReport->SaveFile(logFile);
				}
				wxMessageBox(wxT("Output completed"), wxT("Message box"), wxOK);
				wxBell();
				outputFormatText->SetLabel(wxT("All formats completed"));
				
				FileOpenBttn->Enable(TRUE);
				DestBttn->Enable(TRUE);
				AboutBttn->Enable(TRUE);
				CancelBttn->Enable(TRUE);
			}
}

void mFixTranslator::OnCancelButton(wxCommandEvent& event)
{
	OnCancel(event);
	Destroy();		
}

void mFixTranslator::OnCheckboxLegacyVTK(wxCommandEvent& event)
{
	// Set legacy VTK ASCII output flag here (and enable/disable radiobox)
	if(legacyVTKOutput)
	{
		legacyVTKOutput = FALSE;
		legacyVTKRadioBox->Enable(FALSE);
	} else {
		legacyVTKOutput = TRUE;
		legacyVTKRadioBox->Enable(TRUE); 
	}
}

void mFixTranslator::OnRadioBoxLegacyVTK(wxCommandEvent& event)
{
	if(legacyVTKRadioBox->GetSelection()==0)
	{
		GridWriterFormatFlag = 1;
	} else {
		GridWriterFormatFlag = 2;
	}
}

void mFixTranslator::OnCheckboxSerialXML(wxCommandEvent& event)
{
	// Set serial XML output flag here
	if(serialXMLOutput)
	{
		serialXMLOutput = FALSE;
	} else {
		serialXMLOutput = TRUE;
	}
}

void mFixTranslator::OnCheckboxParallelXML(wxCommandEvent& event)
{
	// Set parallel XML output flag here
	if(parallelXMLOutput)
	{
		parallelXMLOutput = FALSE;
	} else {
		parallelXMLOutput = TRUE;
	}
}

void mFixTranslator::OnCheckboxCGNS(wxCommandEvent& event)
{
	// Set CGNS output flag here
	if(CGNSOutput)
	{
		CGNSOutput = FALSE;
	} else {
		CGNSOutput = TRUE;
	}
}

void mFixTranslator::OnButtonSelectAllOptions(wxCommandEvent& event)
{
	exportGeometry->SetValue(TRUE);
	exportBoundingBox->SetValue(TRUE);
}

void mFixTranslator::OnButtonUnselectAllOptions(wxCommandEvent& event)
{
	exportGeometry->SetValue(FALSE);
	exportBoundingBox->SetValue(FALSE);
}

void mFixTranslator::OnButtonSelectAll(wxCommandEvent& event)
{
	legacyVTKCheckbox->SetValue(TRUE);
	serialXMLCheckbox->SetValue(TRUE);
	parallelXMLCheckbox->SetValue(TRUE);
	cgnsCheckbox->SetValue(TRUE);
	legacyVTKRadioBox->Enable(TRUE);
	serialXMLOutput = TRUE;
	parallelXMLOutput = TRUE;
	CGNSOutput = TRUE;
	legacyVTKOutput = TRUE;
}

void mFixTranslator::OnButtonUnselectAll(wxCommandEvent& event)
{
	legacyVTKCheckbox->SetValue(FALSE);
	serialXMLCheckbox->SetValue(FALSE);
	parallelXMLCheckbox->SetValue(FALSE);
	cgnsCheckbox->SetValue(FALSE);
	legacyVTKRadioBox->Enable(FALSE);
	serialXMLOutput = FALSE;
	parallelXMLOutput = FALSE;
	CGNSOutput = FALSE;
	legacyVTKOutput = FALSE;
}

void mFixTranslator::OnSelectAllOutputVariablesButton(wxCommandEvent& event)
{
	if(sp1Exists)
	{
		VoidFractionVariable->SetValue(TRUE);
	}
	if(sp2Exists)
	{
		GasPressureVariable->SetValue(TRUE);
		P_StarVariable->SetValue(TRUE);
	}
	if(sp3Exists)
	{
		GasVelocityVariable->SetValue(TRUE);
	}
	if(sp4Exists)
	{
		SolidPhaseVelocityVariable->SetValue(TRUE);
	}
	if(sp5Exists)
	{
		SolidPhaseDensityVariable->SetValue(TRUE);
	}
	if(sp6Exists)
	{
		TemperaturesVariable->SetValue(TRUE);
	}
	if(sp7Exists)
	{
		GasSpeciesVariable->SetValue(TRUE);
		SolidPhaseSpeciesVariable->SetValue(TRUE);
	}
	if(sp8Exists)
	{
		Theta_MVariable->SetValue(TRUE);
	}
	if(sp9Exists)
	{
		ScalarsVariable->SetValue(TRUE);
	}
	if(spaExists)
	{
		NumReactionRatesVariable->SetValue(TRUE);
	}
	int MaxTS = DetermineMaxTimeSteps();
	wxString strMaxTS;
	strMaxTS.Printf(wxT("%i"), MaxTS);
	BeginTSBox->SetValue(wxT("0"));
	EndTSBox->SetValue(strMaxTS);
}

void mFixTranslator::OnUnselectAllOutputVariablesButton(wxCommandEvent& event)
{
	if(sp1Exists)
	{
		VoidFractionVariable->SetValue(FALSE);
	}
	if(sp2Exists)
	{
		GasPressureVariable->SetValue(FALSE);
		P_StarVariable->SetValue(FALSE);
	}
	if(sp3Exists)
	{
		GasVelocityVariable->SetValue(FALSE);
	}
	if(sp4Exists)
	{
		SolidPhaseVelocityVariable->SetValue(FALSE);
	}
	if(sp5Exists)
	{
		SolidPhaseDensityVariable->SetValue(FALSE);
	}
	if(sp6Exists)
	{
		TemperaturesVariable->SetValue(FALSE);
	}
	if(sp7Exists)
	{
		GasSpeciesVariable->SetValue(FALSE);
		SolidPhaseSpeciesVariable->SetValue(FALSE);
	}
	if(sp8Exists)
	{
		Theta_MVariable->SetValue(FALSE);
	}
	if(sp9Exists)
	{
		ScalarsVariable->SetValue(FALSE);
	}
	if(spaExists)
	{
		NumReactionRatesVariable->SetValue(FALSE);
	}
	int MaxTS = DetermineMaxTimeSteps();
	wxString strMaxTS;
	strMaxTS.Printf(wxT("%i"), MaxTS);
	BeginTSBox->SetValue(wxT("0"));
	EndTSBox->SetValue(strMaxTS);
}

void mFixTranslator::OnTextCtrlBeginningTS(wxCommandEvent& event)
{
	wxString wxTemp;
      	
	wxTemp = BeginTSBox->GetValue();
	if(atoi((const char *)wxTemp.c_str()) < 0)
	{
		wxMessageBox(wxT("Beginning timestep cannot be less than 0!"));
		wxBell();
		BeginTSBox->SetValue(wxT("0"));
	}
	BeginTSVal = atoi((const char *)wxTemp.c_str());
}

void mFixTranslator::OnTextCtrlEndingTS(wxCommandEvent& event)
{
	wxString wxTemp;
	wxTemp = EndTSBox->GetValue();
	if(atoi((const char *)wxTemp.c_str()) > DetermineMaxTimeSteps())
	{
		wxString Warning;
		Warning.Printf(wxT("Ending range cannot be greater than the total number of timesteps in any file.\nThis project contains a maximum of %i timesteps."), DetermineMaxTimeSteps());
		wxMessageBox(Warning);
		wxBell();
		int MaxAvailableTS = DetermineMaxTimeSteps();
		Warning.Printf(wxT("%i"),MaxAvailableTS);
		EndTSBox->SetValue(Warning);
	}
	EndTSVal = atoi((const char *)wxTemp.c_str());
}

void mFixTranslator::OnCheckboxEnableSpecificTSRange(wxCommandEvent& event)
{
	if(EnableSpecificTSRange->GetValue()==TRUE)
	{
		BeginTSLabel->Enable(TRUE);
		BeginTSBox->Enable(TRUE);
		EndTSBox->Enable(TRUE);
		BeginTSBox->SetValue(wxT("0"));
		int MaxTS = DetermineMaxTimeSteps();
		wxString strMaxTS;
		strMaxTS.Printf(wxT("%i"), MaxTS);
		EndTSBox->SetValue(strMaxTS);
		EndTSLabel->Enable(TRUE);
	} else {
		BeginTSLabel->Enable(FALSE);
		BeginTSBox->Enable(FALSE);
		EndTSBox->Enable(FALSE);
		BeginTSBox->SetValue(wxT(""));
		EndTSBox->SetValue(wxT(""));
		EndTSLabel->Enable(FALSE);
	}
}

void mFixTranslator::UpdateMaxTS(wxCommandEvent& event)
{
	int MaxTS = DetermineMaxTimeSteps();
	wxString strMaxTS;
	strMaxTS.Printf(wxT("%i"), MaxTS);
	BeginTSBox->SetValue(wxT("0"));
	EndTSBox->SetValue(strMaxTS);
}
