/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/

// --- VE-Suite Includes --- //
#include <ves/conductor/util/CORBAServiceList.h>

// --- My Includes --- //
#include "IntStoves_UI_Dialog.h"
#include "GLCanvasWrapper.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/Command.h>

#include <ves/conductor/UIPluginBase.h>

// --- wxWidgets Includes --- //
#include <wx/dc.h>
#include <wx/sizer.h>
#include <wx/textctrl.h>
#include <wx/combobox.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/button.h>
#include <wx/checkbox.h>

// --- C/C++ Libraries --- //
#include <iostream>
#include <iomanip>
#include <fstream>
#include <ostream>

using namespace ves::conductor;

BEGIN_EVENT_TABLE( IntStoves_UI_Dialog, UIDialog )
//EVT_COMBOBOX( NUMBAFFSEL_COMBOBOX, IntStoves_UI_Dialog::_onNumBafSel )
EVT_COMBOBOX( ACTBAFFSEL_COMBOBOX, IntStoves_UI_Dialog::_onActBafSel )
EVT_COMBOBOX( REMOVEBAFF_COMBOBOX,IntStoves_UI_Dialog::_onRemoveBaff )
//EVT_BUTTON( DESIGN_BUTTON, IntStoves_UI_Dialog::_onDesignStove )
//EVT_BUTTON( ADDBAFF_BUTTON, IntStoves_UI_Dialog::_onAddBaff )
EVT_BUTTON( REMOVEBAFF_BUTTON, IntStoves_UI_Dialog::_onRemoveBaff )
EVT_SPINCTRL( CHANGE_DEPTH, IntStoves_UI_Dialog::SetDepth )
EVT_BUTTON( UPDATE_PARAMS, IntStoves_UI_Dialog::UpdateParams )
EVT_CHECKBOX( VECTOR_CHECKBOX, IntStoves_UI_Dialog::ShowVectors )
EVT_CHECKBOX( CONTOUR_CHECKBOX, IntStoves_UI_Dialog::ShowContour )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
IntStoves_UI_Dialog::IntStoves_UI_Dialog()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
IntStoves_UI_Dialog::IntStoves_UI_Dialog(
    wxWindow* parent,
    int id,
    long* numbaffles,
    std::vector< double >* baffle1,
    std::vector< double >* baffle2,
    std::vector< double >* baffle3,
    std::vector< double >* baffle4,
    std::vector< double >* baffle5,
    std::vector< double >* baffle6,
    std::vector< double >* baffle7 )
    :
    UIDialog( static_cast< wxWindow* >( parent ),
              id,
              wxT( "IntStoves" ) ),
    p_numbaffles( numbaffles ),
    p_baffle1( baffle1 ),
    p_baffle2( baffle2 ),
    p_baffle3( baffle3 ),
    p_baffle4( baffle4 ),
    p_baffle5( baffle5 ),
    p_baffle6( baffle6 ),
    p_baffle7( baffle7 )
{
    m_numbaffles = 0;

    vectors = 0;
    contour = 0;

    m_command = ves::open::xml::CommandPtr( new ves::open::xml::Command() );

    _buildPage();
}

////////////////////////////////////////////////////////////////////////////////
IntStoves_UI_Dialog::~IntStoves_UI_Dialog()
{
    mCORBAService->CleanUp();
}
////////////////////////////////////////////////////////////////////////////////
void IntStoves_UI_Dialog::_buildPage()
{
    for( int i = 0; i < 7; ++i )
    {
        baffnums[i] << (i + 1);
        activebaff[i] << (i + 1);
        actbaffdrawn[i] = false;
    }

    //_numbaffsel = new wxComboBox(this, NUMBAFFSEL_COMBOBOX , wxT("Select Number of Baffles"),
    //							wxDefaultPosition, wxDefaultSize, 7, baffnums, wxCB_DROPDOWN);

    //_designButton = new wxButton(this, DESIGN_BUTTON, wxT("Design Stove"));

    //wxStaticBox* _baffnumSBox = new wxStaticBox(this,-1, wxT("Baffle Selection"));
    //wxStaticBoxSizer* _baffnumGroup = new wxStaticBoxSizer(_baffnumSBox,wxVERTICAL);


    //_baffnumGroup->Add(_numbaffsel, 1, wxALIGN_LEFT|wxEXPAND);
    //_baffnumGroup->Add(_designButton, 1, wxALIGN_LEFT);

    wxStaticBox* _baffdef = new wxStaticBox(this,-1, wxT("Baffle Definition"));
    wxStaticBoxSizer* _baffdefGroup = new wxStaticBoxSizer(_baffdef,wxHORIZONTAL);

    wxStaticText* _startposxLabel       = new wxStaticText(this, -1, wxT("Start X"), wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE );
    wxStaticText* _startposyLabel       = new wxStaticText(this, -1, wxT("Start Y"), wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE );
    wxStaticText* _directionLabel       = new wxStaticText(this, -1, wxT("Direction"), wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE );
    wxStaticText* _lengthLabel          = new wxStaticText(this, -1, wxT("Length"), wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE );
    wxStaticText* _depthLabel           = new wxStaticText(this, -1, wxT("Depth"), wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE );

    _baffdefGroup->Add(_startposxLabel, 2, wxALIGN_CENTER_HORIZONTAL);
    _baffdefGroup->Add(5, 5, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);
    _baffdefGroup->Add(_startposyLabel, 2, wxALIGN_CENTER_HORIZONTAL);
    _baffdefGroup->Add(5, 5, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);
    _baffdefGroup->Add(_directionLabel, 2, wxALIGN_CENTER_HORIZONTAL);
    _baffdefGroup->Add(5, 5, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);
    _baffdefGroup->Add(_lengthLabel, 2, wxALIGN_CENTER_HORIZONTAL);
    _baffdefGroup->Add(_depthLabel, 3, wxALIGN_CENTER_HORIZONTAL);

    wxBoxSizer* _baffall = new wxBoxSizer(wxVERTICAL);
    _baffall->Add(_baffdefGroup, 1, wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

    for ( int i=0; i<7; i++ )
    {
        wxString temp( wxT("Baffle Number " ) );
        temp << (i+1);
        _baff[i] = new wxStaticBox(this,-1, temp );
        _baffGroup[i] = new wxStaticBoxSizer(_baff[i],wxHORIZONTAL);
        _startposx[i] = new wxTextCtrl(this, -1,wxT("0"),wxDefaultPosition,wxSize(40,25),wxTE_READONLY,wxDefaultValidator);
        _startposy[i] = new wxTextCtrl(this, -1,wxT("0"),wxDefaultPosition,wxSize(40,25),wxTE_READONLY,wxDefaultValidator);
        _direction[i] = new wxTextCtrl(this, -1,wxT("0"),wxDefaultPosition,wxSize(40,25),wxTE_READONLY,wxDefaultValidator);
        _length[i]    = new wxTextCtrl(this, -1,wxT("0"),wxDefaultPosition,wxSize(40,25),wxTE_READONLY,wxDefaultValidator);
        _depth[i]	  = new wxSpinCtrl(this, CHANGE_DEPTH ,wxT("1"),wxDefaultPosition,wxSize(70,25),wxSP_ARROW_KEYS|wxSP_WRAP,1,12,1);
        _baffGroup[i]->Add(_startposx[i], 1, wxALIGN_CENTER_HORIZONTAL);
        _baffGroup[i]->Add(_startposy[i], 1, wxALIGN_CENTER_HORIZONTAL);
        _baffGroup[i]->Add(_direction[i], 1, wxALIGN_CENTER_HORIZONTAL);
        _baffGroup[i]->Add(_length[i], 1, wxALIGN_CENTER_HORIZONTAL);
        _baffGroup[i]->Add(_depth[i], 1, wxALIGN_CENTER_HORIZONTAL);
        _baffall->Add(_baffGroup[i], 1, wxALIGN_CENTER_HORIZONTAL);
            _baff[i]->Enable( false );
            _startposx[i]->Enable( false );
            _startposy[i]->Enable( false );
            _direction[i]->Enable( false );
            _length[i]->Enable( false );
            _depth[i]->Enable( false );
    }
  //_activebaffsel = new wxComboBox(this, ACTBAFFSEL_COMBOBOX , wxT("Select the Active Baffle"),
	//							wxDefaultPosition, wxDefaultSize, 7, activebaff, wxCB_DROPDOWN);
  //_addbafButton = new wxButton(this, ADDBAFF_BUTTON, wxT("Create New Baffle"));
  //_removebafButton = new wxButton(this, REMOVEBAFF_BUTTON, wxT("Remove Active Baffle"));
  _removebafCombo = new wxComboBox(this, REMOVEBAFF_COMBOBOX , wxT("Select the Baffle to Remove"),
								wxDefaultPosition, wxDefaultSize, 0, activebaff, wxCB_DROPDOWN);
  _updateButton = new wxButton(this, UPDATE_PARAMS, wxT("Update"));
  m_closeButton = new wxButton(this, wxID_OK, wxT("Close"));

  _rightset = new wxBoxSizer(wxVERTICAL);
  _rightset->Add(_baffall, 9, wxALIGN_CENTER_HORIZONTAL);
  //_rightset->Add(_activebaffsel, 1, wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
  _rightset->AddSpacer(15);
  _rightset->Add(_removebafCombo, 1, wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
  _rightset->AddSpacer(5);
  //_rightset->Add(_addbafButton, 1, wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
  //_rightset->Add(_removebafButton, 1, wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
  //_rightset->AddSpacer(15);

    m_vectorCheckBox = new wxCheckBox(this, VECTOR_CHECKBOX, wxT("Show Vectors (velocity)"), wxDefaultPosition, wxDefaultSize, 0);
    m_contourCheckBox = new wxCheckBox(this, CONTOUR_CHECKBOX, wxT("Show Contour (temperature)"), wxDefaultPosition, wxDefaultSize, 0);

    m_checkBoxes = new wxBoxSizer(wxHORIZONTAL);
    m_checkBoxes->Add(m_vectorCheckBox, 1, wxALIGN_CENTER_HORIZONTAL|wxEXPAND); 
    m_checkBoxes->Add(m_contourCheckBox, 1, wxALIGN_CENTER_HORIZONTAL|wxEXPAND); 

    _rightset->Add(m_checkBoxes, 1, wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
    _rightset->AddSpacer(25);

    m_buttons = new wxBoxSizer(wxHORIZONTAL);
    m_buttons->Add(_updateButton, 1, wxALIGN_CENTER_HORIZONTAL|wxEXPAND);    
    m_buttons->Add(m_closeButton, 1, wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

    _rightset->Add(m_buttons, 1, wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

    wxBoxSizer* _leftset = new wxBoxSizer(wxVERTICAL);
    mCanvasWrapper = new GLCanvasWrapper( this, _leftset );

    wxBoxSizer* _mainSizer = new wxBoxSizer(wxHORIZONTAL);

    _mainSizer->Add(_leftset, 2, wxALIGN_LEFT);
    _mainSizer->Add(_rightset, 1, wxALIGN_RIGHT);

    //set this flag and let wx handle alignment
    SetAutoLayout(true);
    //assign the group to the panel
    SetSizer(_mainSizer);
    _mainSizer->Fit(this);
}
////////////////////////////////////////////////////////////////////////////////
bool IntStoves_UI_Dialog::TransferDataFromWindow()
{
    return true;
}
////////////////////////////////////////////////////////////////////////////////
bool IntStoves_UI_Dialog::TransferDataToWindow()
{
    return true;
}
////////////////////////////////////////////////////////////////////////////////
void IntStoves_UI_Dialog::Lock( bool l )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
/*
void IntStoves_UI_Dialog::_onNumBafSel(wxCommandEvent& event)
{
	for ( int i=0; i<7; i++)
	{
	  if ( i <= _numbaffsel->GetSelection() )
	  {
		_baff[i]->Enable( true );
		_startposx[i]->Enable( true );
		_startposy[i]->Enable( true );
		_direction[i]->Enable( true );
		_length[i]->Enable( true );
		_depth[i]->Enable( true );
	  }
	  else
	  {
	    if ( actbaffdrawn[i] )
		{
			_removeBaff(i);
			actbaffdrawn[i] = false;
		}

		_baff[i]->Enable( false );
		_startposx[i]->Enable( false );
		_startposy[i]->Enable( false );
		_direction[i]->Enable( false );
		_length[i]->Enable( false );
		_depth[i]->Enable( false );
	  }
	}

	_rebuildActBaffSel();
}
*/
/*
////////////////////////////////////////////////////////////////////////////////
void IntStoves_UI_Dialog::_onDesignStove(wxCommandEvent& event)
{
    
    _designCanvas->SetCurrent( *(_designCanvas->GetContext()) );
    _designCanvas->_draw();  
  
    for ( int i=0; i<7; i++ )
    {
	    if ( _startposx[i]->GetLastPosition() > 0 )
        {
		    _reDrawBaff(i);	
        }
    }
    _designCanvas->SwapBuffers();
    
}
*/
////////////////////////////////////////////////////////////////////////////////
void IntStoves_UI_Dialog::_onActBafSel( wxCommandEvent& event )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void IntStoves_UI_Dialog::_onAddBaff()
{	
    float* tempGrid = mCanvasWrapper->GetGridInfo();
    int* actpt1 = mCanvasWrapper->GetGridPoints( 1 );
    int* actpt2 = mCanvasWrapper->GetGridPoints( 2 );
    
	if( tempGrid[ 0 ] != tempGrid[ 1 ]  && tempGrid[ 2 ] != tempGrid[ 3 ] )
	{
        return;
    }

	(m_numbaffles) += 1;
	for ( int i=0; i<(m_numbaffles); i++ )
	{
		_baff[i]->Enable( true );
		_startposx[i]->Enable( true );
		_startposy[i]->Enable( true );
		_direction[i]->Enable( true );
		_length[i]->Enable( true );
		_depth[i]->Enable( true );
	}
	wxString txt;
	//if ( _designCanvas->actpt1[0] != -1 && _designCanvas->actpt2[0] != -1 && !actbaffdrawn[_activebaffsel->GetSelection()])
	if ( actpt1[0] != -1 && actpt2[0] != -1 && !actbaffdrawn[(m_numbaffles)-1])
	{	
        if ( actpt1[0] == actpt2[0] || actpt1[1] == actpt2[1] )
		{
			if ( actpt1[0] < actpt2[0])
			{
				txt << actpt1[0];
				_startposx[(m_numbaffles)-1]->SetValue(txt); txt.clear();
				txt << actpt1[1];
				_startposy[(m_numbaffles)-1]->SetValue(txt); txt.clear();
				txt << 0;
				_direction[(m_numbaffles)-1]->SetValue(txt); txt.clear();
				txt <<  actpt2[0] - actpt1[0];
				_length[(m_numbaffles)-1]->SetValue( txt ); txt.clear();
			}
			else if ( actpt1[0] > actpt2[0])
			{
				txt << actpt2[0];
				_startposx[(m_numbaffles)-1]->SetValue(txt); txt.clear();
				txt << actpt2[1];
				_startposy[(m_numbaffles)-1]->SetValue(txt); txt.clear();
				txt << 0;
				_direction[(m_numbaffles)-1]->SetValue(txt); txt.clear();
				txt << actpt1[0] - actpt2[0];
				_length[(m_numbaffles)-1]->SetValue( txt ); txt.clear();
			}
			else if ( actpt1[1] < actpt2[1])
			{
				txt << actpt2[0];
				_startposx[(m_numbaffles)-1]->SetValue(txt); txt.clear();
				txt << actpt2[1];
				_startposy[(m_numbaffles)-1]->SetValue(txt); txt.clear();
				txt << 3;
				_direction[(m_numbaffles)-1]->SetValue(txt); txt.clear();
				txt << actpt2[1] - actpt1[1];
				_length[(m_numbaffles)-1]->SetValue( txt ); txt.clear();
			}
			else if ( actpt1[1] > actpt2[1])
			{
				txt << actpt1[0];
				_startposx[(m_numbaffles)-1]->SetValue(txt); txt.clear();
				txt << actpt1[1];
				_startposy[(m_numbaffles)-1]->SetValue(txt); txt.clear();
				txt << 3;
				_direction[(m_numbaffles)-1]->SetValue(txt); txt.clear();
				txt << actpt1[1] - actpt2[1];
				_length[(m_numbaffles)-1]->SetValue( txt ); txt.clear();
			}
			mCanvasWrapper->DrawNewBaffle();
			actbaffdrawn[(m_numbaffles)-1] = true;
		}
    }
    
    _rebuildActBaffSel();
    SetBaffleData();
}
////////////////////////////////////////////////////////////////////////////////
void IntStoves_UI_Dialog::_reDrawBaff(int index)
{
    if( m_numbaffles > 0 )
    {
        long int temp1;
        _startposx[index]->GetValue().ToLong( &temp1 );
        long int temp2;
        _startposy[index]->GetValue().ToLong( &temp2 );
        long int temp3;
        _direction[index]->GetValue().ToLong( &temp3 );
        long int temp4;
        _length[index]->GetValue().ToLong( &temp4 );
        mCanvasWrapper->RedrawBaffle( temp1, temp2, temp3, temp4, index);	
    }
}
////////////////////////////////////////////////////////////////////////////////
void IntStoves_UI_Dialog::_onRemoveBaff(wxCommandEvent& event)
{
	//if ( _activebaffsel->GetSelection() != -1)
	if ( actbaffdrawn[_removebafCombo->GetSelection()])
	{
		int index = _removebafCombo->GetSelection();
		_removeBaff(index);
		_reOrganizeBaffs();
		
	}
}
////////////////////////////////////////////////////////////////////////////////
void IntStoves_UI_Dialog::SetDepth( wxSpinEvent& event )
{
    SetBaffleData();
}
////////////////////////////////////////////////////////////////////////////////
void IntStoves_UI_Dialog::_removeBaff(int index)
{
    long int temp1;
    _startposx[index]->GetValue().ToLong( &temp1 );
    long int temp2;
    _startposy[index]->GetValue().ToLong( &temp2 );
    long int temp3;
    _direction[index]->GetValue().ToLong( &temp3 );
    long int temp4;
    _length[index]->GetValue().ToLong( &temp4 );
    mCanvasWrapper->RemoveBaffle( temp1, temp2, temp3, temp4 );	

	_startposx[index]->Clear();
	_startposy[index]->Clear();
	_direction[index]->Clear();
	_length[index]->Clear();
	actbaffdrawn[index] = false;
}
////////////////////////////////////////////////////////////////////////////////
void IntStoves_UI_Dialog::_rebuildActBaffSel()
{
    /*
	_rightset->Remove(_removebafCombo);
	delete _removebafCombo;
	_removebafCombo = new wxComboBox(this, REMOVEBAFF_COMBOBOX , wxT("Select the Baffle to Remove"),
							wxDefaultPosition, wxDefaultSize, (m_numbaffles), activebaff, wxCB_DROPDOWN);
	_rightset->Insert(2,_removebafCombo, 1, wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	Refresh();
*/
    _removebafCombo->Clear();
    for( int i = 0; i < m_numbaffles; ++i )
    {
        wxString number;
        int tempNum = i+1;
        number << tempNum;
        _removebafCombo->Append(number);
    }
    _removebafCombo->SetValue( wxT("Select the Baffle to Remove") );

	static bool test = false;
	int flag = 0;
	if ( test )
	{
		flag = 1;
		test = false;
	}
	else
	{
		flag = -1;
		test = true;
	}
	   
	wxSize temp = GetSize();
	temp.SetHeight( temp.GetHeight()+flag );
	temp.SetWidth( temp.GetWidth()+flag );
	SetSize( temp );
    
    mCanvasWrapper->DrawCanvas();
/*#ifndef __WXMAC__
	_designCanvas->SetCurrent( *(_designCanvas->GetContext()) );
#else
	_designCanvas->SetCurrent();
#endif
	_designCanvas->_draw();  
  
	for ( int i=0; i<m_numbaffles; i++ )
	{
		_reDrawBaff(i);	  
	}
	_designCanvas->SwapBuffers();*/
}
////////////////////////////////////////////////////////////////////////////////
void IntStoves_UI_Dialog::_reOrganizeBaffs()
{
	(m_numbaffles) -= 1;

	for( int i=0; i<(m_numbaffles); i++ )
	{
		if ( !actbaffdrawn[i] && actbaffdrawn[i+1] )
		{
			_startposx[i]->SetValue(_startposx[i+1]->GetValue());
			_startposy[i]->SetValue(_startposy[i+1]->GetValue());
			_direction[i]->SetValue(_direction[i+1]->GetValue());
			_length[i]->SetValue( _length[i+1]->GetValue());
			_depth[i]->SetValue( _depth[i+1]->GetValue());
			actbaffdrawn[i] = true;
			actbaffdrawn[i+1] = false;
			_startposx[i+1]->Clear();
			_startposy[i+1]->Clear();
			_direction[i+1]->Clear();
			_length[i+1]->Clear();
			_depth[i+1]->SetValue(1);
		}
		/*else if ( i == (m_numbaffles)-1 )
		{
			_startposx[i]->Clear();
			_startposy[i]->Clear();
			_direction[i]->Clear();
			_length[i]->Clear();
			_depth[i]->SetValue(1);
			actbaffdrawn[i] = false;
		}*/
	}
	for ( int i=0; i<7; i++ )
	{
		if ( i >= (m_numbaffles) )
		{
			_startposx[i]->SetValue( wxT("0") );
			_startposy[i]->SetValue( wxT("0") );
			_direction[i]->SetValue( wxT("0") );
			_length[i]->SetValue( wxT("0") );
			_depth[i]->SetValue( wxT("1") );

			_baff[i]->Enable( false );
			_startposx[i]->Enable( false );
			_startposy[i]->Enable( false );
			_direction[i]->Enable( false );
			_length[i]->Enable( false );
			_depth[i]->Enable( false );
		}
	}
	_rebuildActBaffSel();
    SetBaffleData();
}
////////////////////////////////////////////////////////////////////////////////
int IntStoves_UI_Dialog::GetStartX( int index )
{
    long int temp1;
    _startposx[index]->GetValue().ToLong( &temp1 );
    return temp1;
}
////////////////////////////////////////////////////////////////////////////////
int IntStoves_UI_Dialog::GetStartY( int index )
{
    long int temp1;
    _startposy[index]->GetValue().ToLong( &temp1 );
    return temp1;
}
////////////////////////////////////////////////////////////////////////////////
int IntStoves_UI_Dialog::GetDirection( int index )
{
    long int temp1;
    _direction[index]->GetValue().ToLong( &temp1 );
    return temp1;
}
////////////////////////////////////////////////////////////////////////////////
int IntStoves_UI_Dialog::GetLength( int index )
{
    long int temp1;
    _length[index]->GetValue().ToLong( &temp1 );
    return temp1;
}
////////////////////////////////////////////////////////////////////////////////
int IntStoves_UI_Dialog::GetDepth( int index )
{
    return _depth[index]->GetValue();
}
////////////////////////////////////////////////////////////////////////////////
void IntStoves_UI_Dialog::SetBaffleData()
{
    for( size_t i=0; i<7; ++i )
    {
        command_name=std::string("BAFFLE_UPDATE");

        baffleParams.clear();
        baffleParams.push_back( i );
        baffleParams.push_back( GetStartX(i) );
        baffleParams.push_back( GetStartY(i) );
        baffleParams.push_back( GetDirection(i) );
        baffleParams.push_back( GetLength(i) );
        baffleParams.push_back( GetDepth(i) );

        ves::open::xml::DataValuePairPtr baffleParams_DVP( new ves::open::xml::DataValuePair() );
        baffleParams_DVP->SetData(std::string("baffleParams"),baffleParams);
        m_command->AddDataValuePair( baffleParams_DVP );
    
        SendCommandsToXplorer();
        ClearParameters();
    }
}
////////////////////////////////////////////////////////////////////////////////
void IntStoves_UI_Dialog::UpdateParams(wxCommandEvent& event)
{
	mUIPluginBase->SetActiveModel();

	(*p_numbaffles) = m_numbaffles;

	std::vector<double> temp[7];

    long int temp1;
	for ( int i=0; i<7; i++ )
	{
		temp[i].clear();
        _startposx[i]->GetValue().ToLong( &temp1 );
		temp[i].push_back(temp1);
        _startposy[i]->GetValue().ToLong( &temp1 );
		temp[i].push_back( temp1 );
        _direction[i]->GetValue().ToLong( &temp1 );
		temp[i].push_back( temp1 );
        _length[i]->GetValue().ToLong( &temp1 );
		temp[i].push_back( temp1 );
		temp[i].push_back( _depth[i]->GetValue() );
	}

	(*p_baffle1).clear();
	(*p_baffle1) = temp[0];

	(*p_baffle2).clear();
	(*p_baffle2) = temp[1];
	 
	(*p_baffle3).clear();
	(*p_baffle3) = temp[2];
	  
	(*p_baffle4).clear();
	(*p_baffle4) = temp[3];
	  
	(*p_baffle5).clear();
	(*p_baffle5) = temp[4];

	(*p_baffle6).clear();
	(*p_baffle6) = temp[5];

	(*p_baffle7).clear();
	(*p_baffle7) = temp[6];
}
////////////////////////////////////////////////////////////////////////////////
int IntStoves_UI_Dialog::GetNumBaffles()
{
    return m_numbaffles;
}
////////////////////////////////////////////////////////////////////////////////
void IntStoves_UI_Dialog::ShowVectors(wxCommandEvent& event)
{
    if( m_vectorCheckBox->IsChecked() )
    {
        vectors = 1;
    }
    else
    {
        vectors = 0;
    }

    command_name=std::string("SHOW_VECTORS");

    ves::open::xml::DataValuePairPtr vector_DVP( new ves::open::xml::DataValuePair() );
    vector_DVP->SetData(std::string("vectors"),vectors);
    m_command->AddDataValuePair( vector_DVP );
    
    SendCommandsToXplorer();
    ClearParameters();
}
////////////////////////////////////////////////////////////////////////////////
void IntStoves_UI_Dialog::ShowContour(wxCommandEvent &event)
{
    if( m_contourCheckBox->IsChecked() )
    {
        contour = 1;
    }
    else
    {
        contour = 0;
    }

    command_name=std::string("SHOW_CONTOUR");

    ves::open::xml::DataValuePairPtr contour_DVP( new ves::open::xml::DataValuePair() );
    contour_DVP->SetData(std::string("contour"),contour);
    m_command->AddDataValuePair( contour_DVP );
    
    SendCommandsToXplorer();
    ClearParameters();
}
////////////////////////////////////////////////////////////////////////////////
void IntStoves_UI_Dialog::SendCommandsToXplorer()
{
   m_command->SetCommandName(command_name);
   mCORBAService->SendCommandStringToXplorer(m_command);
}
////////////////////////////////////////////////////////////////////////////////
void IntStoves_UI_Dialog::ClearParameters()
{
   parameters.clear();
   command_name.clear() ;
}
////////////////////////////////////////////////////////////////////////////////
